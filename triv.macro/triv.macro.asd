;;;; triv.macro.asd

(asdf:defsystem #:triv.macro
  :serial t
  :description "Small collection of helper functions related to macro"
  :author "That Guy <invalid.email@example.com>"
  :license "Public Domain"
  :components ((:file "package")
               (:file "triv.macro")))

