;;;; parzival.lisp

(in-package #:parzival)


;;; forms with names beginning with << will RETURN A PARSER
;;; and forms beginning and ending in a > ARE A PARSER
;;; e.g. <<foo returns a parser while <goo< is a parser


(defmacro <def< (name parser &optional docstring)
  `(progn
     (defvar ,name ,parser)
     (defun ,name (stream) ,docstring (funcall ,name stream))))

(defun <<result (x)
  (lambda (stream) (values x t stream)))


(<def< <fail<
       (lambda (stream) (values nil nil stream))
       "Consumes nothing from input and fails the parse.")

(<def< <item<
       (lambda (stream) (values (read-char stream) t stream))
       "Consumes exactly one item from input and results in that item.")

(<def< <peek<
       (lambda (stream) (values (peek-char stream) t stream))
       "Results in next item from the input without consuming it.")


(defmacro <<if ((var parser stream) then else)
  "Binds the result of the parser on the stream to var and runs the combinator in then. If the parse fials the combinator else is run instead."
  (let ((ok? (gensym))
        (stream2 (gensym)))
    `(multiple-value-bind (,var ,ok? ,stream2) (funcall ,parser ,stream)
       (if ,ok?
           (funcall ,then ,stream2)
           (funcall ,else ,stream2)))))

(defmacro <<when ((var parser stream) form)
  "Binds the result of parser on stream to var and runs the form. Fails otherwise."
  `(<<if (,var ,parser ,stream) ,form <fail<))

(defun <<rewinding (parser)
  "Turns a parser into a rewinding parser. I.e. If the parse fails, the stream is rewound to its state from before the parse so that other parsers can continue from there."
  (lambda (s)
    (let ((s (replay-on s)))
      (<<if (res parsers)
            (lambda (s)
              (funcall (<<result res) (recover-source s)))
            (lambda (s)
              (funcall <fail< (rewind s)))))))

(defun <<bind (p f)
  "Performs a parse p and returns a new parser that results from applying f to the result of p. If p fails, then so does (<<bind p f)."
  (lambda (stream)
    (<<when (result p stream)
            (funcall f result))))

(defun <<= (p f &rest fs)
  "Just like bind but with a chain of functions. Each function accepts the result of the parse from the previous step and returns a new parser.  If any intermediate parser fails, the whole chain fails."
  (if fs
      (apply #'<<= (cons (<<bind p f) fs))
      (<<bind p f)))

(defun <<and (p1 p2 &rest ps)
  "Just like <<= but where parse results are ignored.  I.e. Applies each parser in sequence, ignoring any intermediate results. The result (<<and p1 p2 ... pn) is the result of pn."
  (if ps
      (apply #'<<and (cons (<<bind p1 (lambda (ignore) p2)) ps))
      (<<bind p1 (lambda (ignore) p2))))

(defun <<map (f p)
  "Turns a parser that results in  x into a parser that results in (funcall f  x)."
  (lambda (s)
    (<<when (val p s) (<<result (funcall f val)))))

;; fails if p fails
(defun <<cons (x p)
  "If the parser p results in y then the parser (<<cons x p) results in (cons x y). If p fails, then so does (<<cons x p)"
  (<<map (lambda (xs) (cons x xs)) p))

;; succeeds with (x) even if p fails, otherwise (cons x result-of-p)
(defun <<cons? (x p)
  "Just like <<cons except that (<<cons? x p) always succeeds. In the case of p failing, (<<cons? x p) results in (cons x nil)."
  (lambda (s)
    (<<if (val p s)
          (<<result (cons x val))
          (<<result (cons x nil)))))

(defun <<many (p)
  "Runs the parser p zero or more times, resulting in of list of parsed values."
  (<<bind p (lambda (x) (<<cons? x (<<many p)))))

(defun <<many1 (p)
  "Like <<many but fails if p does not succeed at least once."
  (<<bind p (lambda (x) (<<cons x (<<many p)))))

(defun <<sat (pred)
  "(<<sat pred) is parser that reads one item from input and succeeds if (pred item) is true."
  (<<bind <item<
       (lambda (c) (if (funcall pred c)
                       (<<result c)
                       <fail<))))

(defun <<peek-sat (pred)
  "(<<peek-sat pred) is like (<<sat pred) but doesn't consume the item if (pred item) is false."
  (<<bind <peek<
          (lambda (c) (if (funcall pred c)
                          <item<
                          <fail<))))

(defun <<char (c)
  "(<<char c) consumes an item from input and succeds if that item is exactly the character c."
  (<<sat (lambda (x) (char-equal x c))))

(defun <<peek-char (c)
  "Like <<char but wont consume the input if the input is not equal to c."
  (<<peek-sat (lambda (x) (char-equal x c))))

(defun <<string (str)
  "Parses exactly the string str, resulting in that str on success."
  (<<map (lambda (ignore) str)
         (apply #'<<and (loop for c across str collect (<<peek-char c)))))

;; some notes/thoughts
;; 1. <<many should take care to fail with the stream intact
;; 2. Should there be peek / nonnpeek versions of each of the utility parsers?


(<def< <uppercase< (<<sat #'upper-case-p))
(<def< <lowercase< (<<sat #'lower-case-p))
(<def< <alphanum< (<<sat #'alphanumericp))
(<def< <letter< (<<sat #'alpha-char-p))
(<def< <space< (<<char #\Space))
(<def< <spaces< (<<many1 <space<))
(<def< <newline< (<<char #\Newline))

(defun digit-p (c)
  (and (alphanumericp c)
       (not (alpha-char-p c))))

(<def< <digit< (<<sat #'digit-p))

(defun read-from-char-list (l)
  (read-from-string (concatenate 'string l)))

(<def< <nat< (<<map #'read-from-char-list (<<many1 <digit<)))


(defun <<plus (p1 p2)
  (lambda (stream)
    (let ((stream (replay-on stream)))
      (<<if (result p1 stream)
            (<<result result)
            (lambda (s)
              (rewind s)
              (funcall p2 s))))))

(defun <<? (p)
  (<<plus p (<<result '())))

(<def< <int<
       (<<bind (<<? (<<char #\-))
               (lambda (neg?)
                 (<<map (lambda (num) (if neg? (* -1 num) num))
                        <nat<))))

(<def< <real<
       (<<bind (<<many1 <digit<)
               (lambda (whole-digits)
                 (<<map (lambda (frac-digits)
                          (read-from-char-list (append whole-digits frac-digit)))
                        (<<? (<<and (<<char #\.)
                                    (<<cons #\. (<<many1 <digit<))))))))

(defun <<or (p1 p2 &rest ps)
  (if ps
      (<<plus p1 (apply #'<<or (cons p2 ps)))
      (<<plus p1 p2)))


(defun <<sep-by (val-p sep-p)
  (<<bind val-p
          (lambda (val)
            (<<and sep-p
                   (<<cons? val (<<sep-by val-p sep-p))))))
