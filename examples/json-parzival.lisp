(defpackage "json-parzival"
  (:use :cl :parzival))

(in-package "json-parzival")

(defvar <json-null< (<<map (lambda (null) :null)
                           (<<string "null")))

(defvar <json-bool< (<<map (lambda (bool) (equal bool "true"))
                           (<<plus (<<string "true") (<<string "false"))))

(defvar <json-num< <real<)

(defvar <json-string<
  (<<char-brackets #\"
                   (<<to-string (<<* (<<asat (not (eql it #\")))))
                   #\"))

(defun <json-array< (stream) (funcall <json-array< stream))
(defun <json-object< (stream) (funcall <json-object< stream))

(defvar <json-value<
  (<<or #'<json-object< #'<json-array< <json-string< <json-num< <json-bool< <json-null<))

(defvar <comma-sep< (<<strip (<<char #\,)))

(defvar <json-array<
  (<<brackets (<<strip (<<char #\[))
              (<<sep-by <json-value< <comma-sep<)
              (<<strip (<<char #\]))))

(defvar <json-object-pair<
  (<<bind (<<brackets <whitespace<
                      <json-string<
                      (<<strip (<<char #\:)))
          (lambda (key)
            (<<map (lambda (value) (cons key value))
                   <json-value<))))

(defvar <json-object<
  (<<brackets (<<strip (<<char #\{))
              (<<sep-by <json-object-pair< <comma-sep<)
              (<<strip (<<char #\}))))




