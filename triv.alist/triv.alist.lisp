;;;; triv.alist.lisp

(in-package #:triv.alist)

;;------------------------------------------------------------
;; triv

(defun assocr (item alist &key (key nil keyp) (test nil testp) 
                            (test-not nil notp))
  (cdr (apply #'assoc item alist (append (when keyp (list :key key)) 
                                         (when testp (list :test test))
                                         (when notp (list test-not))))))

(defun clean-alist (alist)
  (loop 
     :with seen = nil
     :for e in alist     
     :when (and (consp e) (not (member (car e) seen))) 
     :collect (progn (push (car e) seen) e)))

(defun alist->plist (alist)
  (loop 
     :for (i . j) :in alist 
     :collect i :into x 
     :collect j :into x
     :finally (return x)))

(defun plist->alist (plist)
  (loop
     :with tmp = nil
     :for e :in plist :for i :from 0
     :if (evenp i) :do (setf tmp e)
     :else :collect (cons tmp e)))

;;------------------------------------------------------------

