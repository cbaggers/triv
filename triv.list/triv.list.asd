;;;; triv.list.asd

(asdf:defsystem #:triv.list
  :serial t
  :description "Small collection of helper functions related to lists"
  :author "That Guy <invalid.email@example.com>"
  :license "Public Domain"
  :components ((:file "package")
               (:file "triv.list")))

