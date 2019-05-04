;;;; A Toy parser for a most of JSON.
;;;; Specificially, double-quote characters are not escaped in strings
;;;; And the full range of numbers defined in the JSON specification are
;;;; not supported.  This is file is intended to be a demonstration

(defpackage "json-parzival"
  (:use :cl :parzival))

(in-package "json-parzival")

;;; Parses and returns JSON's null value
(defvar <json-null< (<<map (lambda (null) :null)
                           (<<string "null")))

;;; Parses and returns JSON's boolean values, true => T, false => NIL
(defvar <json-bool< (<<map (lambda (bool) (equal bool "true"))
                           (<<plus (<<string "true") (<<string "false"))))

;;; Parses a subset of JSON's real number expressions using parzival's built-in
;;; real number parser
(defvar <json-num< <real<)

;;; Parses most of the strings that can be represented in JSON, excepting only
;;; those strings that contain an escaped double-quote symbol
(defvar <json-string<
  (<<char-brackets #\"
                   (<<to-string (<<* (<<asat (not (eql it #\")))))
                   #\"))


;;; For the functions, <json-array< and <json-object<, we defer completing the
;;; definition until after <json-value< has been written. This will raise
;;; warnings during compilation. The reason for this odd behavior is that
;;; <json-array< and <json-object are both defined in terms of <json-value<,
;;; which is itself defined in terms of <json-object< and <json-array<.  
(defun <json-array< (stream) (funcall <json-array< stream))
(defun <json-object< (stream) (funcall <json-object< stream))

;;; This is the main JSON parser, and will parse any stream containing JSON values.
(defvar <json-value<
  (<<or #'<json-object< #'<json-array< <json-string< <json-num< <json-bool< <json-null<))

;;; A utility parser to match a comma surrounded by whitespace
(defvar <comma-sep< (<<strip (<<char #\,)))


;;; A JSON array is a comma separated list of <json-value<'s, surrounded by [ and ].
(defvar <json-array<
  (<<brackets (<<strip (<<char #\[))
              (<<sep-by <json-value< <comma-sep<)
              (<<strip (<<char #\]))))

;;; A utility to turn a string into a KEYWORD. It is probably not general purpose.
(defun make-keyword-symbol (str)
  (read-from-string (format nil ":~a" str)))

;;; A JSON object is a collectin of key-value pairs. This parser matchs and
;;; retunrs such a pair. Ignoring whitespace, a pair is a string followed by a :
;;; followed by any JSON value. Here, strings in the key position are
;;; transformed into KEYWORDs.  The pair is returned as a (cons key value).
(defvar <json-object-pair<
  (<<bind (<<brackets <whitespace<
                      <json-string<
                      (<<strip (<<char #\:)))
          (lambda (key)
            (<<map (lambda (value) (cons (make-keyword-symbol key) value))
                   <json-value<))))

;;; Finally, a JSON object is a comma separated list of key-value pairs,
;;; surrounded by { and }.
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
