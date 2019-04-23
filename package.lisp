;;;; package.lisp

(defpackage #:parzival
  (:use #:cl #:replay-streams)
  (:export
   #:>>if
   #:>>when
   #:>def>
   #:>>result
   #:>fail>
   #:>item>
   #:>>=
   #:>>cons
   #:>>cons?
   #:>>many
   #:>>sat
   #:>>char
   #:>uppercase>
   #:>lowercase>
   #:>alphanum>
   #:>letter>
   #:>digit>
   #:>>or))

