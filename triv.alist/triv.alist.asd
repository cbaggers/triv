;;;; triv.alist.asd

(asdf:defsystem #:triv.alist
  :serial t
  :description "Small collection of helper functions related to association lists"
  :author "That Guy <invalid.email@example.com>"
  :license "Public Domain"
  :components ((:file "package")
               (:file "triv.alist")))

