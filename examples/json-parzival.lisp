;;;; A Toy parser for a most of JSON.
;;;; Specificially, double-quote characters are not escaped in strings
;;;; And the full range of numbers defined in the JSON specification are
;;;; not supported.  This is file is intended to be a demonstration

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

(defun make-keyword-symbol (str)
  (read-from-string (format nil ":~a" str)))


(defvar <json-object-pair<
  (<<bind (<<brackets <whitespace<
                      <json-string<
                      (<<strip (<<char #\:)))
          (lambda (key)
            (<<map (lambda (value) (cons (make-keyword-symbol key) value))
                   <json-value<))))

(defvar <json-object<
  (<<brackets (<<strip (<<char #\{))
              (<<sep-by <json-object-pair< <comma-sep<)
              (<<strip (<<char #\}))))



;;; An Example
(defun run-the-example ()
  (with-open-file (input "foo.json")
    (let ((stream (make-instance 'replay-streams:character-input-replay-stream
                                 :source input)))
      (parse stream <json-value<))))
;; should produce:

;; (((:NAME . "Boutade")
;;   (:LANGUAGES ((:LANG . "Common Lisp") (:PROFICIENCY . :NULL) (:LOVESIT . T))
;;               ((:LANG . "Rust") (:PROFICIENCY . 0.8) (:LOVESIT . T)
;;                                 (:ISASHAMEDTOLOVEIT . T))
;;               ((:LANG . "Haskell") (:PROFICIENCY . 0.5)
;;                                    (:LOVESIT . "sometimes, in some ways")))
;;   (:PIZZAORDER "Tempeh Italian Sausage" "Spinach" "Mushrooms"
;;                "Red Pepper Flakes")
;;   (:ISCOOL) (:ISFUNNY) (:THINKSPEOPLEARELAUGHING . T)
;;   (:BEHONEST_THINKSPEOPLEARELAUGHING))
;;  ((:NAME . "Goofist")
;;   (:LANGUAGES
;;    ((:LANG . "Common Lisp") (:PROFICIENCY "over" 9000) (:LOVESIT . T))
;;    ((:LANG . "Rust") (:PROFICIENCY . -1) (:LOVESIT . T)
;;                      (:ISASHAMEDTOLOVEIT . T))
;;    ((:LANG . "Haskell") (:PROFICIENCY . -1)
;;                         (:LOVESIT . "i cannot tell a lie")))
;;   (:PIZZAORDER "Blue Stilton" "Walnuts" "Pork Sausage" "Apple Slices")
;;   (:ISCOOL . T) (:ISFUNNY . T) (:THINKSPEOPLEARELAUGHING . T)
;;   (:BEHONEST_THINKSPEOPLEARELAUGHING . T)))
