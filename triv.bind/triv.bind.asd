;;;; triv.bind.asd

(asdf:defsystem #:triv.bind
  :serial t
  :description "Small collection of helper functions related to binding"
  :author "That Guy <invalid.email@example.com>"
  :license "Public Domain"
  :components ((:file "package")
               (:file "triv.bind")))

