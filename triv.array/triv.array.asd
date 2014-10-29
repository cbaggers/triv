;;;; triv.array.asd

(asdf:defsystem #:triv.array
  :serial t
  :description "Small collection of helper functions related to arrays"
  :author "That Guy <invalid.email@example.com>"
  :license "Public Domain"
  :components ((:file "package")
               (:file "triv.array")))

