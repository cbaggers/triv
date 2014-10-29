;;;; triv.macros.lisp

(in-package #:triv.macros)


;;------------------------------------------------------------
;; CREDITS - fare

(defun alist->hash-table (alist &key (test #'eql))
  (loop
     :with h = (make-hash-table :test test :size (length alist))
     :for (k . v) :in alist
     :do (setf (gethash k h) v)
     :finally (return h)))

(defun hash-table->alist (table)
  (loop :for key :being :the :hash-keys :of table :using (:hash-value value)
     :collect (cons key value)))

(defun make-hashset (&key (test 'eql) list set)
  (let ((h (make-hash-table :test test)))
    (dolist (x list)
      (setf (gethash x h) t))
    (when set
      (loop :for x :being :the :hash-keys :in set :do (setf (gethash x h) t)))
    h))

;;------------------------------------------------------------
;; triv

(defun hash-table->plist (table)
  (loop :for key :being :the :hash-keys :of table :using (:hash-value value)
     :collect key :collect value))

;;------------------------------------------------------------

(defmacro hash ((&key (test 'eql) (size 10)
                      (rehash-size 1.5) (rehash-threshold 1)
                      (hash-function nil) (weakness nil) synchronized) 
                &rest keys-vals-plist)
  (let ((h (gensym "new-hashmap")))
    `(let ((,h (make-hash-table :test ,test :size ,size 
                                :rehash-size ,rehash-size
                                :rehash-threshold ,rehash-threshold
                                :hash-function ,hash-function
                                :weakness ,weakness 
                                :synchronized ,synchronized)))
       ,@(loop
            :with tmp = nil
            :for e :in keys-vals-plist
            :for i :from 0
            :if (evenp i) :do (setf tmp e)
            :collect `(setf (gethash ,tmp ,h) ,e))
       ,h)))


