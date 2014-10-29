;;;; triv.bit.asd

(asdf:defsystem #:triv.bit
  :serial t
  :description "Small collection of helper functions related to bit level operations"
  :author "That Guy <invalid.email@example.com>"
  :license "Public Domain"
  :components ((:file "package")
               (:file "triv.bit")))

