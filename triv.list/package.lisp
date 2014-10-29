;;;; package.lisp

(defpackage #:triv.list
  (:use #:cl)
  (:export :flatten
           :group
           :last1
           :starts-with
           :find-anywhere
           :unique-find-if-anywhere
           :partition-if
           :mappend
           :listify))

