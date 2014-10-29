;;;; triv.sequence.asd

(asdf:defsystem #:triv.sequence
  :serial t
  :description "Small collection of helper functions related to sequences"
  :author "That Guy <invalid.email@example.com>"
  :license "Public Domain"
  :components ((:file "package")
               (:file "triv.sequence")))

