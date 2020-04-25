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


;; TODO refactor parse to detect how best to create a replay stream, automatically
(defun parse (stream parser &optional string-as-stream-p)
  "Parse STREAM with PARSER. If STRING-AS-STREAM-P then STREAM can be a string."
  (if string-as-stream-p
      (funcall parser (make-instance 'replay-streams:static-text-replay-stream :text stream))
      (funcall parser stream)))

;;; A private utility macro for defining a defvar and a defun at the same time,
;;; intended for use in defining parsers as the result of other parsers, but
;;; letting them be called like functions too if necessary.

(defmacro <<def (name parser &optional docstring)
  "Define a parser. <<DEF will define both a DEFUN called NAME and a
DEFVAR called name that simply holds the symbol NAME. This makes it
easier to write recursive parsers and to combine parsers by name.

PARSER is any expression that returns a parser. i.e. a function of
accepting a REPLAY-STREAM and returning three values. The first value
is the result of the parse, the second value is a success indicator,
and the thrid value is the stream stream.  The stream will be left in
whatever state it was in when the parse stopped, either successfully
or not."
  `(progn
     (defvar ,name ',name)
     (defun ,name (stream) ,docstring (funcall ,parser stream))))


;;; The CORE PARSERS out of which all other parsers are built! A most solemn
;;; section of code. Have you properly prepared yourself to read on?

(defun <<result (value)
  "Creates a parser that results in VALUE, having consumed nothing on the input stream."
  (lambda (stream) (values value t stream)))


(<<def <fail<
       (lambda (stream) (values nil nil stream))
       "Consumes nothing from input and fails the parse.")

(<<def <peek<
       (lambda (stream)
         (if (peek-char nil stream nil nil)
             (funcall (<<result (peek-char nil stream nil nil)) stream)
             (funcall <fail< stream)))
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
  (lambda (stream)
    (let ((chkpt (checkpoint stream)))
      (<<if (result parser1 stream)
            (progn
              (free-checkpoint stream chkpt)
              (<<result result))
            (progn
              (rewind-to stream chkpt)
              parser2)))))

(defun <<until (parser)
  "Consumes the stream one character at a time until PARSER succeeds. The parse
  value of (<<UNTIL PARSER) is a list of the characters consumed, ending in the
  value parsed by PARSER."
  (let ((collected nil))
    (labels ((rec (stream)
               (let ((chkpt (replay-streams:checkpoint stream)))
                 (<<if (result parser stream)
                       (progn
                         (replay-streams:free-checkpoint stream chkpt)
                         (<<result (reverse (cons result collected))))
                       (progn
                         (replay-streams:rewind-to stream chkpt)
                         (<<bind <item<
                                 (lambda (item)
                                   (push item collected)
                                   #'rec)))))))
      #'rec)))

(defun <<ignore-until (parser)
  "Just like <<UNTIL except it does not build up intermediate results. Results
   in the result of PARSER."
  (labels ((rec (stream)
                (let ((chkpt (checkpoint stream)))
                  (<<if (result parser stream)
                        (progn
                          (free-checkpoint stream chkpt)
                          (<<result result))
                        (progn
                          (rewind-to stream chkpt)
                          (<<and <item< #'rec))))))
          #'rec))

(defun <<filter (parser pred)
  "Condition a successful parse on a predicate"
  (<<bind parser
          (lambda (parsed)
            (if (funcall pred parsed)
                (<<result parsed)
                <fail<))))

(defun <<or (&rest parsers)
  "Tries each parser one after the other, rewinding the input stream after each
   failure, and resulting in the first successful parse."
  (cond ((null parsers) <fail<)
        ((null (cdr parsers)) (car parsers))
        (t
         (<<plus (car parsers) (apply #'<<or (cdr parsers))))))

(defun <<~ (parser)
  "Turns a parser into a rewinding parser. I.e. If the PARSER would fail, then
   the input stream is first rewound before the fail occurrs."
  (<<plus parser <fail<))


(defun <<? (parser)
  "Makes a parser optional. I.e. if the PARSER would fail, it instead succeeds
  with NIL and the input stream is rewound."
  (<<plus parser (<<result nil)))


(defun <<any-char (str)
  "Makes a parser that accepts and results in any of the characters in the
  provided string"
  (apply #'<<or (loop for c across str collect (<<char c))))

(defun <<any-string (&rest strings)
  "Makes a parser that accepts and results in any of the provided strings"
  (apply #'<<or (mapcar #'<<string strings)))


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


(defmacro <<let (bindings expression)
  "Introduce bindings for the results of several consequtive parses. If one of
  them fails then the whole expression fails. BINDINGS should be analogus to a
  let expression, i.e. (variable parser) pairs. EXPRESSION should be any
  expression that returns a parser - it can make use of the bound variables.

  Any binding variable whose SYMBOL-NAME starts with an underscore is declared
  as IGNORE. This is handy if you want to run a parser in sequence but don't 
  care about its result value - you can binding to a variable like _VAR and the
  compiler won't complain.
  "
  (if (null bindings) expression
      `(<<bind ,(cadar bindings)
               (lambda (,(caar bindings))
                 ,(when (ignorable-symbol-p (caar bindings))
                    (list 'declare (list 'ignore (caar bindings))))
                 (<<let ,(cdr bindings) ,expression)))))


(defun <<and (parser1 parser2 &rest parsers)
  "Just like <<BIND but where parse results are ignored. I.e. Applies each
   parser in sequence, ignoring any intermediate results. The result (<<AND P1
   P2 ... PN) is the result of PN."
  (if parsers
      (apply #'<<and (cons (<<bind parser1 (returning parser2)) parsers))
      (<<bind parser1 (returning parser2))))


(defun <<first (parser other &rest others-still)
  "A bit like <<AND in reverse. Returns the parse result of the first parser,
   but only if the other parsers all succeed."
  (<<bind parser
          (lambda (first)
            (apply #'<<and (append (list other)
                                   others-still
                                   (list (<<result first)))))))


(defun <<ending (parser)
  "Creates a parser that succeeds if PARSER succeeds and the end of the input has been reached."
  (<<bind parser
          (lambda (result)
            (<<map (returning result) <eof<))))

;;; PARSING INDIVIDUAL ITEMS from the stream. The basic parser thats of any real
;;; use is <<sat. It lets you check that a stream item meets some kind of
;;; condition, and fails to parse if it does not. The follwing section contains
;;; a large number of utilities that parse one input item at a time.

(defun <<sat (pred)
  "(<<SAT PRED) is parser that reads one item from input and succeeds
  if (FUNCALL PRED ITEM) is true and fails otherwise."
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

(defmacro <<asat (form)
  "An anaphoric macro for defining a <<sat type parser where IT is bound to the
  character being tested"
  (let ((it (intern (symbol-name 'it))))
    `(<<sat (lambda (,it) ,form))))


(defmacro <<def-item-sat (name pred  &optional docstring)
  ;; This is a less general version of <<~def, it is only to be used to define
  ;; parsers that operate on a single character
  (let ((peeking-version (intern (concatenate 'string "<~" (subseq (string name) 1)))))
    `(progn
       (<<def ,name (<<sat ,pred) ,docstring)
       (<<def ,peeking-version (<<~sat ,pred) ,docstring))))


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

(<<def-item-sat <tab< (lambda (c) (eql c #\Tab))
                "Parses a single tab character")

(<<def-item-sat <return< (lambda (c) (eql c #\Return))
                "Parses a single carriage return character")

(<<def-item-sat <linefeed< (lambda (c) (eql c #\Linefeed))
                "Parses a single linefeed character")



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

(defun <<cons-map (parser x)
  "If PARSER results in Y, then (<<CONS-MAP PARSER X) results in (CONS Y X)"
  (<<map (lambda (y) (cons y x)) parser))

(defun <<map-list (x parser)
  "If PARSER results in Y then (<<MAP-LIST X PARSER) results in (X Y)"
  (<<map (lambda (y) (list x y)) parser))

(defun <<list-map (parser x)
  (<<map (lambda (y) (list y x)) parser))

(defun <<map-append (xs parser)
  "If PARSER results in YS, assumed to be list, then (<<MAP-APPEND XS PARSER) results in
  (append XS YS)"
  (<<map (lambda (ys) (append xs ys)) parser))

;;; PARSING SEQUENCES

(defun <<cons (head-parser tail-parser)
  "Returns a parser that conses the result of parsing HEAD-PARSER to the result of
  parsing TAIL-PARSER, fails if either fails"
  (<<bind head-parser
          (lambda (head)
            (<<map (lambda (tail) (cons head tail)) tail-parser))))


(defun <<list (&rest parsers)
  (if (null parsers) (<<result nil)
      (<<cons (car parsers) (apply #'<<list (cdr parsers)))))

(defun <<* (parser)
  "Runs the parser PARSER zero or more times, resulting in of list of parsed values."
  (<<bind (<<? parser)
          (lambda (result)
            (if (null result) (<<result result)
                (<<map-cons result (<<* parser))))))


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


;;; DELIMITED VALUES

(defun <<sep-by (value-parser separator-parser)
  "Parses a sequence of values with VALUE-PARSER ignoring a separator that is
  parsed with SEPARATOR-PARSER. E.g. (<<SEP-BY <NAT< (<<CHAR #\,)) would parse
  a string like '1,2,3,4' and result in a list (1 2 3 4)"
  (<<cons value-parser (<<* (<<and separator-parser value-parser))))


(defun <<sep-by* (value-parser separator-parser)
  "Just like <<SEP-BY, but matches an empty sequence"
  (<<? (<<sep-by value-parser separator-parser)))

(defun <<brackets (left center right)
  (<<and left (<<bind center
                      (lambda (bracketed-value)
                        (<<map (returning bracketed-value) right)))))


(defun <<char-brackets (left-char center right-char)
  (<<brackets (<<char left-char) center (<<char right-char)))


;;; VALUE PARSERS. The following section contains utilities for parsing common
;;; values like strings or numbers.


(defun <<string (str)
  "Parses exactly the string STR, resulting in STR on success."
  (cond ((zerop (length str))
         (<<result nil))
        ((= 1 (length str))
         (<<~char (aref str 0)))
        (t 
         (<<map (returning str)
                (apply #'<<and (loop for c across str collect (<<~char c)))))))


(defun <<~string (str)
  "Parses exactly the string STR, resulting in STR. Rewinding version."
  (cond ((zerop (length str))
         (<<result nil))
        ((= 1 (length str))
         (<<~char (aref str 0)))
        (t
         (<<map (returning str)
                (<<~ (apply #'<<and (loop for c across str collect (<<char c))))))))

(defun <<to-string (parser)
  "If the result of PARSER is a list of characters, transform it into a string.
  Signals an error of the result X cannot be called in (concatenate 'string X)"
  (<<map (lambda (result) (concatenate 'string result)) parser))


(<<def <whitespace< (<<* (<<or <space< <newline< <return< <linefeed< <tab<))
       "Parses zero or more whitespace characters, returning them as a list")


(<<def <word< (<<to-string (<<+ <letter<))
        "Parses a sequence of one or more letters characters, resulting in
        a string containing them.")


(defun read-from-char-list (l)
  (read-from-string (concatenate 'string l)))


(<<def <nat< (<<map #'read-from-char-list (<<+ <~digit<))
        "Parses a natural number.")


(<<def <int<
        (<<bind (<<? (<<char #\-))
                (lambda (neg?)
                  (<<map (lambda (num) (if neg? (* -1 num) num))
                         <nat<)))
        "Parses an integer")


(<<def <frac<
       (<<or (<<bind (<<char #\.)
                     (lambda (dot)
                       (<<map (lambda (frac)
                                (read-from-char-list (cons dot frac)))
                              (<<+ <digit<))))
             (<<result 0))
       "Parses a fractional number in decimal format - e.g. .3234 or .002")


(<<def <real<
       (<<bind (<<? (<<char #\-))
               (lambda (neg?)
                 (<<bind <int<
                         (lambda (whole)
                           (<<map (lambda (frac)
                                    (* (if neg? -1 1) (+ whole frac)))
                                  <frac<)))))
       "Parses a real number")


;;; LANGUAGE UTILITIES

(defun <<strip (parser &optional (strip <whitespace<))
  "Returns parses PARSER that may be surrounded on the left and the right by
   zero or more STRIP. The contented parsed by STRIP is ignored"
  (<<brackets strip parser strip))


;;; UTILITY FUNCTIONS

(defun returning (x)
  "Returns a lambda that returns x no matter what it gets as an argument"
  (lambda (ignore) x))


(defun ignorable-symbol-p (symb)
  "Returns true if a symbol name starts with _"
  (eql #\_ (elt (symbol-name symb) 0)))
