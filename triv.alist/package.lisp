;;;; package.lisp

(defpackage #:triv.alist
  (:use #:cl)
  (:export :assocr
           :clean-alist
           :alist->plist
           :plist->alist))

