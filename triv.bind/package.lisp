;;;; package.lisp

(defpackage #:triv.bind
  (:use #:cl)
  (:export #:dbind
           #:if-let
           #:when-let
           #:when-let*
           #:lambda-list-split
           #:letf))

