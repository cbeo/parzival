;;;; parzival.lisp

(in-package #:parzival)

;;;; NAMING CONVENTIONS
;;;;
;;;; Forms with names beginning with << return parsers. Forms beginning
;;;; and ending in a > are themselves parsers. E.g. <<foo returns a parser while <goo< is
;;;; a parser. A ~ in the name indicates a "rewinding" parser that will safely
;;;; rewind the input stream before failing. Finally, a ? at the end of a name
;;;; indicates an "optional" parser, one that will either succeed or will result
;;;; in NIL and will safely rewind the stack.

;;;; WHAT IS A PARSER?
;;;;
;;;; In parzival, a parser is a function that accepts an input stream and
;;;; returns three values. The first value is the parse result, which can be any
;;;; lisp value (numbers, lists, strings, anything your little heart desires).
;;;; As a matter of terminology: if the first value is X, we say that the parser
;;;; "results in X". The second value returned by a parser is T if the parse
;;;; succeeded and is NIL if the parse failed. We return this second value so
;;;; that parsers can meaningfully result in NIL but still be considered to have
;;;; succeded. The third value is the stream which may have been transformed or
;;;; altered in some way as a result of passing it to the parser. Mostly,
;;;; however, you can ignore the second and third values returned by a parser.
;;;; Parzival includes machinery (in the form of higher order functions and a
;;;; few macros) to abstract the second and third values away. When you are
;;;; building your own parsers you usually need only concentrate on the parse
;;;; results.

;;; The PARSE function. Runs parsers.

(defun parse (stream parser &optional string-as-stream-p)
  "Parse STREAM with PARSER. If STRING-AS-STREAM-P then STREAM can be a string."
  (if string-as-stream-p
      (funcall parser (make-string-input-stream stream))
      (funcall parser stream)))


;;; A private utility macro for defining a defvar and a defun at the same time,
;;; intended for use in defining parsers as the result of other parsers, but
;;; letting them be called like functions too if necessary.

(defmacro <<def (name parser &optional docstring)
  `(progn
     (defvar ,name ,parser)
     (defun ,name (stream) ,docstring (funcall ,name stream))))


;;; The CORE PARSERS out of which all other parsers are built! A most solemn
;;; section of code. Have you properly prepared yourself to read on?

(defun <<result (value)
  "Creates a parser that results in VALUE, having consumed nothing on the input stream."
  (lambda (stream) (values value t stream)))


(<<def <fail<
       (lambda (stream) (values nil nil stream))
       "Consumes nothing from input and fails the parse.")

(<<def <peek<
       (lambda (stream) (values (peek-char nil stream nil nil) t stream))
       "A pseudo-parser that peeks at the next item without consuming it. Useful
       for building efficient look-ahead of one item.")


(<<def <item<
       (lambda (stream)
         (if (peek-char nil stream nil nil)
             (funcall (<<result (read-char stream)) stream)
             (funcall <fail< stream)))
       "Results in next item from the input stream, or fails.")


(<<def <eof<
       (lambda (stream)
         (if (peek-char nil stream nil nil)
             (values nil nil stream)
             (values t t stream)))
       "A parser that results in T if the end of the input stream has been
reached, fails otherwise")


;;; The following two macros make defining combinators much nicer. Both let you
;;; choose what to do with with the input stream based on the result of a a
;;; previous parse.

(defmacro <<if ((var parser stream) then else)
  "Binds the result of the parser on the stream to var and runs the combinator
in then. If the parse fails the combinator else is run instead."
  (let ((ok? (gensym))
        (stream2 (gensym)))
    `(multiple-value-bind (,var ,ok? ,stream2) (funcall ,parser ,stream)
       (if ,ok?
           (funcall ,then ,stream2)
           (funcall ,else ,stream2)))))


(defmacro <<when ((var parser stream) form)
  "Binds the result of parser on stream to var and runs the form. Fails otherwise."
  `(<<if (,var ,parser ,stream) ,form <fail<))


;;; The <<PLUS COMBINATOR is vital, and gives us amazing powers to choose our
;;; own future!  This section defines <<plus and uses it to define some nice utilities.

(defun <<plus (parser1 parser2)
  "Introduces a choice between two parsers. If PARSER1 succeeds then its result
is used. If PARSER1 fails then the stream is rewound and tried again with
PARSER2."
  (lambda (stream)
    (let ((stream (replay-on stream)))
      (<<if (res parser1 stream)
            (lambda (s)
              (funcall (<<result res) (recover-source s)))
            (lambda (s)
              (rewind s)
              (funcall parser2 s))))))



(defun <<or (parser1 parser2 &rest parsers)
  "Tries each parser one after the other, rewinding the input stream after each
failure, and resulting in the first successful parse."
  (if parsers
      (<<plus parser1 (apply #'<<or (cons parser2 parsers)))
      (<<plus parser1 parser2)))


(defun <<~ (parser)
  "Turns a parser into a rewinding parser. I.e. If the PARSER would fail, then
the input stream is first rewound before the fail occurrs."
  (<<plus parser <fail<))


(defun <<? (parser)
  "Makes a parser optional. I.e. if the PARSER would fail, it instead succeeds
  with NIL and the input stream is rewound."
  (<<plus parser (<<result nil)))


(defmacro <<~def (name parser &optional docstring)
  ;; a version of <<def that also makes rewinding parsers.
  (let ((rewinding-name (make-symbol (concatenate 'string "<~" (subseq (string name) 1)))))
    `(progn
       (<<def ,name ,parser ,docstring)
       (<<def ,rewinding-name (<<~ ,name) ,docstring))))


;;; <<BIND LETS US CHAIN PARSERS together in different ways. Fundamentally, the
;;; <<bind combinator lets us use the result of one parse to create a new
;;; parser. <<bind also propgates errors through these "chains of parsers",
;;; letting us fail the first time any parser in the chain fails. The following
;;; section defines <<bind and some utilities that derive from it.

(defun <<bind (parser fn)
  "Parses a stream with PARSER. If PARSER fails then so does (<<BIND PARSER FN).
  If PARSER succeeds with result RESULT then, RESULT is passed to the function
  FN, which is expected to return a parser."
  (lambda (stream)
    (<<when (result parser stream)
            (funcall fn result))))


(defun <<and-then (parser fn &rest fns)
  "Just like bind but with a chain of functions. Each function accepts the
  result of the parse from the previous step and returns a new parser. If any
  intermediate parser fails, the whole chain fails."
  (if fns
      (apply #'<<and-then (cons (<<bind parser fn) fns))
      (<<bind parser fn)))


(defun <<and (parser1 parser2 &rest parsers)
  "Just like <<AND-THEN but where parse results are ignored. I.e. Applies each
   parser in sequence, ignoring any intermediate results. The result (<<AND P1
   P2 ... PN) is the result of PN."
  (if parsers
      (apply #'<<and (cons (<<bind parser1 (lambda (ignore) parser2)) parsers))
      (<<bind parser1 (lambda (ignore) parser2))))


(defun <<ending (parser)
  "Creates a parser that succeeds if PARSER succeeds and the end of the input has been reached."
  (<<bind parser
          (lambda (result)
            (lambda (stream)
              (<<when (eof <eof< stream)
                      (<<result result))))))


;;; PARSING INDIVIDUAL ITEMS from the stream. The basic parser thats of any real
;;; use is <<sat. It lets you check that a stream item meets some kind of
;;; condition, and fails to parse if it does not. The follwing section contains
;;; a large number of utilities that parse one input item at a time.

(defun <<sat (pred)
  "(<<SAT PRED) is parser that reads one item from input and succeeds if (FUNCALL PRED ITEM) is true and fails otherwise."
  (<<bind <item<
          (lambda (c) (if (funcall pred c)
                          (<<result c)
                          <fail<))))


(defun <<~sat (pred)
  "(<<~SAT PRED) is like (<<SAT PRED) but doesn't consume the item if (FUNCALL PRED ITEM) is false."
  (<<bind <peek<
          (lambda (c) (if (funcall pred c)
                          <item<
                          <fail<))))


(defmacro <<def-item-sat (name pred  &optional docstring)
  ;; This is a less general version of <<~def, it is only to be used to define
  ;; parsers that operate on a single character
  (let ((rewinding-name (make-symbol (concatenate 'string "<~" (subseq (string name) 1)))))
    `(progn
       (<<def ,name (<<sat ,pred) ,docstring)
       (<<def ,rewinding-name (<<~sat ,pred) ,docstring))))


(defun <<char (c)
  "(<<CHAR C) consumes an item from input and succeeds if that item is exactly
the character C."
  (<<sat (lambda (x) (eql x c))))


(defun <<~char (c)
  "Like <<CHAR but wont consume the input if the input is not equal to C."
  (<<~sat (lambda (x) (eql x c))))


(defun <<char-equal (c)
  "The case-insensitive version of <<CHAR."
  (<<sat (lambda (x) (char-equal x c))))

(defun <<char-equal (c)
  "The case-insensitive version of <<CHAR."
  (<<~sat (lambda (x) (char-equal x c))))


(<<def-item-sat <uppercase< #'upper-case-p
                "Parses one uppercase alphabet letter.")


(<<def-item-sat <lowercase< #'lower-case-p
                "Parses one lowercase alphabet letter. ")


(<<def-item-sat <alphanum< #'alphanumericp
                "Parses an alphanumeric character.")


(<<def-item-sat <letter< #'alpha-char-p
                "Parses a single alphabetic character.")


(<<def-item-sat <space<  (lambda (c) (eql c #\Space))
                "Parses one space character.")


(<<def-item-sat <newline< (lambda (c) (eql c #\Newline))
                "Parses a single new line character.")


(defun digit-p (c)
  (and (alphanumericp c)
       (not (alpha-char-p c))))


(<<def-item-sat <digit< #'digit-p "Parses a single 0 - 9 digit character.")

;;; <<MAP GIVES YOU NEW RESULTS FROM OLD PARSERS! This section contains a few
;;; utilities built with <<map.

(defun <<map (fn parser)
  "Turns a parser that results in X into a parser that results in (FUNCALL FN X)."
  (lambda (s)
    (<<when (val parser s) (<<result (funcall fn val)))))


(defun <<map-cons (x parser)
  "If the parser PARSER results in Y then the parser (<<MAP-CONS X PARSER) results in
  (CONS X Y). If PARSER fails, then so does (<<MAP-CONS X P)"
  (<<map (lambda (xs) (cons x xs)) parser))


(defun <<map-cons? (x parser)
  "Like <<MAP-CONS except if the parser PARSER fails, then the result is (CONS X NIL)"
  (<<map-cons x (<<? parser)))

;;; PARSING SEQUENCES

(defun <<cons (head-parser tail-parser)
  "Returns a parser that conses the result of parsing HEAD-PARSER to the result of
  parsing TAIL-PARSER, fails if either fails"
  (<<bind head-parser
          (lambda (head)
            (<<map (lambda (tail) (cons head tail)) tail-parser))))

(defun <<* (parser)
  "Runs the parser PARSER zero or more times, resulting in of list of parsed values."
  (<<bind (<<? parser)
          (lambda (val) (if val
                            (<<map-cons val (<<* parser))
                            (<<result nil)))))


(defun <<+ (parser)
  "Like <<* but fails if P does not succeed at least once."
  (<<cons parser (<<* parser)))


(defun <<times (n parser)
  "Builds a parser that will run PARSER exactly N times, returning a list of the
  results."
  (if (<= n 0) (<<result nil)
      (<<cons parser (<<times (1- n) parser))))


(defun <<min-times (n parser)
  "Builds a parser that will run PARSER at least N times, possibly more,
  returning a list of the results."
  (if (<= n 0) (<<* parser)
      (<<cons parser (<<min-times (1- n) parser))))


(defun <<max-times (n parser)
  "Builds a parser that will run PARSER at most N times, possibly fewer,
   returning a list of the results."
  (let ((count 0))
    (<<bind (<<* (<<map (lambda (r) (incf count) r) parser))
            (lambda (results) (if (> count n) <fail<
                                  (<<result results))))))


(defun <<sep-by (value-parser separator-parser)
  "Parses a sequence of values with VALUE-PARSER ignoring a separator that is
  parsed with SEPARATOR-PARSER. E.g. (<<SEP-BY <NAT< (<<CHAR #\,)) would parse
  a string like '1,2,3,4' and result in a list (1 2 3 4)"
  (<<bind value-parser
          (lambda (val)
            (<<or (<<and separator-parser
                         (<<map-cons val (<<sep-by value-parser separator-parser)))
                  (<<result (list val))))))



;;; VALUE PARSERS. The following section contains utilities for parsing common
;;; values like strings or numbers.

(defun <<string (str)
  "Parses exactly the string STR, resulting in STR on success."
  (<<map (lambda (ignore) str)
         (apply #'<<and (loop for c across str collect (<<~char c)))))


(defun <<~string (str)
  "Parses exactly the string STR, resulting in STR. Rewinding version."
  (<<map (lambda (ignore) str)
         (<<~ (apply #'<<and (loop for c across str collect (<<char c))))))


(defun <<to-string (parser)
  "If the result of PARSER is a list of characters, transform it into a string.
  Signals an error of the result X cannot be called in (concatenate 'string X)"
  (<<map (lambda (result) (concatenate 'string result)) parser))


(<<~def <word< (<<to-string (<<+ <letter<))
        "Parses a sequence of one or more alphanumeric characters, resulting in
        a string containing them.")

(defun read-from-char-list (l)
  (read-from-string (concatenate 'string l)))


(<<~def <nat< (<<map #'read-from-char-list (<<+ <digit<))
        "Parses a natural number.")


(<<~def <int<
        (<<bind (<<? (<<char #\-))
                (lambda (neg?)
                  (<<map (lambda (num) (if neg? (* -1 num) num))
                         <nat<)))
        "Parses an integer")



