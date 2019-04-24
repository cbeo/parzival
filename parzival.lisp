;;;; parzival.lisp

(in-package #:parzival)

;;; forms with names beginning with >> will RETURN A PARSER
;;; and forms beginning and ending in a > ARE A PARSER
;;; e.g. >>foo returns a parser while >goo> is a parser


(defmacro >def> (name parser)
  `(progn
     (defvar ,name ,parser)
     (defun ,name (stream) (funcall ,name stream))))

(defun >>result (x)
  (lambda (stream) (values x t stream)))


(>def> >fail> (lambda (stream) (values nil nil stream)))

(>def> >item> (lambda (stream) (values (read-char stream) t stream)))

(>def> >peek> (lambda (stream) (values (peek-char stream) t stream)))


(defmacro >>if ((var parser stream) then else)
  (let ((ok? (gensym))
        (stream2 (gensym)))
    `(multiple-value-bind (,var ,ok? ,stream2) (funcall ,parser ,stream)
       (if ,ok?
           (funcall ,then ,stream2)
           (funcall ,else ,stream2)))))

(defmacro >>when ((var parser stream) form)
  `(>>if (,var ,parser ,stream) ,form >fail>))

(defun >>rewinding (parser)
  (lambda (s)
    (let ((s (replay-on s)))
      (>>if (res parsers)
            (lambda (s)
              (funcall (>>result res) (recover-source s)))
            (lambda (s)
              (funcall >fail> (rewind s)))))))

(defun >>bind (p f)
  (lambda (stream)
    (>>when (result p stream)
            (funcall f result))))

(defun >>= (p f &rest fs)
  (if fs
      (apply #'>>= (cons (>>bind p f) fs))
      (>>bind p f)))

(defun >>and (p1 p2 &rest ps)
  (if ps
      (apply #'>>and (cons (>>bind p1 (lambda (ignore) p2)) ps))
      (>>bind p1 (lambda (ignore) p2))))

(defun >>map (f p)
  (lambda (s)
    (>>when (val p s) (>>result (funcall f val)))))

;; fails if p fails
(defun >>cons (x p)
  (>>map (lambda (xs) (cons x xs)) p))

;; succeeds with (x) even if p fails, otherwise (cons x result-of-p)
(defun >>cons? (x p)
  (lambda (s)
    (>>if (val p s)
          (>>result (cons x val))
          (>>result (cons x nil)))))

(defun >>many (p)
  (>>bind p (lambda (x) (>>cons? x (>>many p)))))

(defun >>many1 (p)
  (>>bind p (lambda (x) (>>cons x (>>many p)))))

(defun >>sat (pred)
  (>>bind >item>
       (lambda (c) (if (funcall pred c)
                       (>>result c)
                       >fail>))))

(defun >>peek-sat (pred)
  (>>bind >peek>
          (lambda (c) (if (funcall pred c)
                          >item>
                          >fail>))))

(defun >>char (c)
  (>>sat (lambda (x) (char-equal x c))))

(defun >>peek-char (c)
  (>>peek-sat (lambda (x) (char-equal x c))))

(defun >>string (str)
  (>>map (lambda (ignore) str)
         (apply #'>>and (loop for c across str collect (>>peek-char c)))))

(>def> >uppercase> (>>sat #'upper-case-p))
(>def> >lowercase> (>>sat #'lower-case-p))
(>def> >alphanum> (>>sat #'alphanumericp))
(>def> >letter> (>>sat #'alpha-char-p))
(>def> >space> (>>char #\Space))
(>def> >spaces> (>>many1 >space>))
(>def> >newline> (>>char #\Newline))

(defun digit-p (c)
  (and (alphanumericp c)
       (not (alpha-char-p c))))

(>def> >digit> (>>sat #'digit-p))

(defun read-from-char-list (l)
  (read-from-string (concatenate 'string l)))

(>def> >nat> (>>map #'read-from-char-list (>>many1 >digit>)))


(defun >>plus (p1 p2)
  (lambda (stream)
    (let ((stream (replay-on stream)))
      (>>if (result p1 stream)
            (>>result result)
            (lambda (s)
              (rewind s)
              (funcall p2 s))))))

(defun >>? (p)
  (>>plus p (>>result '())))

(>def> >int>
       (>>bind (>>? (>>char #\-))
               (lambda (neg?)
                 (>>map (lambda (num) (if neg? (* -1 num) num))
                        >nat>))))

(>def> >real>
       (>>bind (>>many1 >digit>)
               (lambda (whole-digits)
                 (>>map (lambda (frac-digits)
                          (read-from-char-list (append whole-digits frac-digit)))
                        (>>? (>>and (>>char #\.)
                                    (>>cons #\. (>>many1 >digit>))))))))

(defun >>or (p1 p2 &rest ps)
  (if ps
      (>>plus p1 (apply #'>>or (cons p2 ps)))
      (>>plus p1 p2)))


(defun >>sep-by (val-p sep-p)
  (>>bind val-p
          (lambda (val)
            (>>and sep-p
                   (>>cons? val (>>sep-by val-p sep-p))))))
