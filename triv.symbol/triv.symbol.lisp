;;;; triv.symbol.lisp

(in-package #:triv.symbol)

(defun symb (&rest args)
  "This takes a list of symbols (or strings) and outputs one 
   symbol.
   If the input is symbol/s then the output is a regular symbol
   If the input is string/s, then the output is
   a |symbol like this|"
  (values (intern (format nil "~{~a~}" args))))


(defun symb-package (package &rest args)
  "This takes a list of symbols (or strings) and outputs one 
   symbol.
   If the input is symbol/s then the output is a regular symbol
   If the input is string/s, then the output is
   a |symbol like this|"
  (values (intern (format nil "~{~a~}" args) package)))

(defun keyword (&rest args)
  (symb-package :keyword))

(declaim (inline format-symbol))
(defun format-symbol (package control &rest arguments)
  "Constructs a string by applying ARGUMENTS to string designator CONTROL as
if by FORMAT within WITH-STANDARD-IO-SYNTAX, and then creates a symbol named
by that string.

If PACKAGE is NIL, returns an uninterned symbol, if package is T, returns a
symbol interned in the current package, and otherwise returns a symbol
interned in the package designated by PACKAGE."
  (maybe-intern (with-standard-io-syntax
                  (apply #'format nil (string control) arguments))
                package)) 
