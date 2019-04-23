;;;; parzival.lisp

(in-package #:parzival)

;;; forms with names beginning with >> will RETURN A PARSER
;;; and forms beginning and ending in a > ARE A PARSER
;;; e.g. >>foo returns a parser while >goo> is a parser


(defmacro >>if ((var parser stream) then else)
  (let ((ok? (gensym))
        (stream2 (gensym)))
    `(multiple-value-bind (,var ,ok? ,stream2) (funcall ,parser ,stream)
       (if ,ok?
           (funcall ,then ,stream2)
           (funcall ,else ,stream2)))))

(defmacro >>when (lambda-list form)
  `(>>if ,lambda-list ,form >fail>))


(defmacro >def> (name parser)
  `(progn
     (defvar ,name ,parser)
     (defun ,name (stream) (funcall ,name stream))))

(defun >>result (x)
  (lambda (stream) (values x t stream)))


(>def> >fail> (lambda (stream) (values nil nil stream)))

(>def> >item> (lambda (stream) (values (read-char stream) t stream)))


(defun >>= (p f)
  (lambda (stream)
    (>>when (result p stream)
            (funcall f result))))

;; fails if p fails
(defun >>cons (x p)
  (>>= p (lambda (res) (>>result (cons x res)))))


;; succeeds with (x) even if p fails, otherwise (cons x result-of-p)
(defun >>cons? (x p)
  (lambda (s)
    (>>if (val p s)
          (>>result (cons x val))
          (>>result (cons x nil)))))

(defun >>many (p)
  (>>= p (lambda (x) (>>cons? x (>>many p)))))

(defun >>sat (pred)
  (>>= >item>
       (lambda (c) (if (funcall pred c)
                       (>>result c)
                       >fail>))))

(defun >>char (c)
  (>>sat (lambda (x) (char-equal x c))))

(>def> >uppercase> (>>sat #'upper-case-p))
(>def> >lowercase> (>>sat #'lower-case-p))
(>def> >alphanum> (>>sat #'alphanumericp))
(>def> >letter> (>>sat #'alpha-char-p))

(defun digit-p (c)
  (and (alphanumericp c)
       (not (alpha-char-p c))))

(>def> >digit> (>>sat #'digit-p))

(defun >>or (p1 p2)
  (lambda (stream)
    (let ((stream (replay-on stream)))
      (>>if (result p1 stream)
            (>>result result)
            (lambda (s)
              (rewind s)
              (funcall p2 s))))))


