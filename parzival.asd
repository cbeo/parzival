;;;; parzival.asd

(asdf:defsystem #:parzival
  :description "Streaming parser language in Common Lisp."
  :author "Colin Okay <cbeok@protonmail.com>"
  :license  "GPLv3"
  :version "0.4.0"
  :serial t
  :depends-on (#:replay-streams)
  :components ((:file "package")
               (:file "parzival")))

