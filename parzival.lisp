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


;;; A private utility macro for defining a defvar and a defun at the same time,
;;; intended for use in defining parsers as the result of other parsers, but
;;; letting them be called like functions too if necessary.

(defmacro <<def (name parser &optional docstring)
  `(progn
     (defvar ,name ,parser)
     (defun ,name (stream) ,docstring (funcall ,name stream))))


;;; The CORE PARSERS out of which all other parsers are built! A most solemn
;;; section of code. Have you properly prepared yourself to read on?

(defun <<result (x)
  "Creates a parser that results in X, having consumed nothing on the input stream."
  (lambda (stream) (values x t stream)))


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

(defun <<plus (p1 p2)
  "Introduces a choice between two parsers. If P1 succeeds then its result is
used. If P1 fails then the stream is rewound and tried again with P2."
  (lambda (stream)
    (let ((stream (replay-on stream)))
      (<<if (res p1 stream)
            (lambda (s)
              (funcall (<<result res) (recover-source s)))
            (lambda (s)
              (rewind s)
              (funcall p2 s))))))



(defun <<or (p1 p2 &rest ps)
  "Tries each parser one after the other, rewinding the input stream after each
failure, and resulting in the first successful parse."
  (if ps
      (<<plus p1 (apply #'<<or (cons p2 ps)))
      (<<plus p1 p2)))


(defun <<~ (parser)
  "Turns a parser into a rewinding parser. I.e. If the parser would fail input
stream is first rewound before the fail occurrs."
  (<<plus parser <fail<))


(defun <<? (p)
  "Makes a parser optional. I.e. if the parser P would fail, it instead succeeds
  with NIL and the input stream is rewound."
  (<<plus p (<<result nil)))


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

(defun <<bind (p f)
  "Performs a parse P and returns a new parser that results from applying F to
the result of P. If P fails, then so does (<<BIND P F)."
  (lambda (stream)
    (<<when (result p stream)
            (funcall f result))))


(defun <<and-then (p f &rest fs)
  "Just like bind but with a chain of functions. Each function accepts the
result of the parse from the previous step and returns a new parser. If any
intermediate parser fails, the whole chain fails."
  (if fs
      (apply #'<<and-then (cons (<<bind p f) fs))
      (<<bind p f)))


(defun <<and (p1 p2 &rest ps)
  "Just like <<AND-THEN but where parse results are ignored. I.e. Applies each parser
in sequence, ignoring any intermediate results. The result (<<AND P1 P2 ... PN)
is the result of PN."
  (if ps
      (apply #'<<and (cons (<<bind p1 (lambda (ignore) p2)) ps))
      (<<bind p1 (lambda (ignore) p2))))




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

(defun <<map (f p)
  "Turns a parser that results in X into a parser that results in (FUNCALL F X)."
  (lambda (s)
    (<<when (val p s) (<<result (funcall f val)))))


(defun <<map-cons (x p)
  "If the parser P results in Y then the parser (<<MAP-CONS X P) results in
  (CONS X Y). If P fails, then so does (<<MAP-CONS X P)"
  (<<map (lambda (xs) (cons x xs)) p))


(defun <<map-cons? (x p)
  "Like <<MAP-CONS except if the parser P fails, then the result is (CONS X NIL)"
  (<<map-cons x (<<? p)))

;;; PARSING SEQUENCES

(defun <<cons (hd tl)
  "Returns a parser that conses the result of parsing head to the result of
  parsing tail, fails if either fails"
  (<<bind hd
          (lambda (head)
            (<<map (lambda (tail) (cons head tail)) tl))))

(defun <<* (p)
  "Runs the parser P zero or more times, resulting in of list of parsed values."
  (<<bind (<<? p)
          (lambda (val) (if val
                            (<<map-cons val (<<* p))
                            (<<result nil)))))

  ;(<<? (<<cons p (<<* p))))


(defun <<+ (p)
  "Like <<* but fails if P does not succeed at least once."
  (<<cons p (<<* p)))


(defun <<times (n p)
  (if (<= n 0) (<<result nil)
      (<<cons p (<<times (1- n) p))))

(defun <<min-times (n p)
  (if (<= n 0) (<<* p)
      (<<cons p (<<min-times (1- n) p))))

(defun <<max-times (n p)
  (let ((count 0))
    (<<bind (<<* (<<map (lambda (r) (incf count) r) p))
            (lambda (results) (if (> count n) <fail<
                                  (<<result results))))))


(defun <<sep-by (val-p sep-p)
  "Parses a sequence of values ignoring a seperator.
  E.g. (<<sep-by <digit< (<<char #\,)) would parse a string like '1,2,3,4' and
  result the list in a list (#\1 #\2 #\3 #\4)"
  (<<bind val-p
          (lambda (val)
            (<<or (<<and sep-p
                         (<<map-cons? val (<<sep-by val-p sep-p)))
                  (<<result (list val))))))


;;; VALUE PARSERS. The following section contains utilities for parsing common
;;; values like strings or numbers.

(defun <<string (str)
  "Parses exactly the string str, resulting in that str on success."
  (<<map (lambda (ignore) str)
         (apply #'<<and (loop for c across str collect (<<~char c)))))


(defun <<~string (str)
  "Parses exactly the string str, resulting in that str. Rewinding version."
  (<<map (lambda (ignore) str)
         (<<~ (apply #'<<and (loop for c across str collect (<<char c))))))


(defun <<to-string (parser)
  "If the result of PARSER is a list of characters, transform it into a string.
  Signals an error of the result X cannot be called in (concatenate 'string X)"
  (<<map (lambda (result) (concatenate 'string result)) parser))


(<<~def <word< (<<to-string (<<+ <alphanum<))
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



