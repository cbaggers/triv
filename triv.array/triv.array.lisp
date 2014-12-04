;;;; triv.array.lisp

(in-package #:triv.array)

(defun copy-array (array &key (element-type (array-element-type array))
                           (fill-pointer (and (array-has-fill-pointer-p array)
                                              (fill-pointer array)))
                           (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

(defun mapa (function array)
  (let ((result (make-array (array-dimensions array)
                            :element-type (array-element-type array)
                            :fill-pointer (and (array-has-fill-pointer-p array)
                                               (fill-pointer array))
                            :adjustable (adjustable-array-p array))))
    (loop for i below (array-total-size array) do 
         (setf (row-major-aref result i) (funcall function (row-major-aref array i)))
       finally (return result))))

(defun nmapa (function array)
  (loop for i below (array-total-size array) do 
       (setf (row-major-aref array i) (funcall function (row-major-aref array i)))
     finally (return array)))
