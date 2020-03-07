;;;; parzival.asd

(asdf:defsystem #:parzival
  :description "Streaming parser language in Common Lisp."
  :author "Boutade <thegoofist@protonmail.com>"
  :license  "GPLv3"
  :version "0.3.0"
  :serial t
  :depends-on (#:replay-streams)
  :components ((:file "package")
               (:file "parzival")))

