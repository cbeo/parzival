;;;; parzival.asd

(asdf:defsystem #:parzival
  :description "An embedded langauage for nearly monadic stream parser combinators."
  :author "Boutade <thegoofist@protonmail.com>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:replay-streams)
  :components ((:file "package")
               (:file "parzival")))
