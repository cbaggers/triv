;;;; triv.list.lisp

(in-package #:triv.list)

(defun flatten (x)
  "Walks a list tree and flattens it (returns a 1d list 
   containing all the elements from the tree)"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x)
                           (rec (cdr x) acc))))))
    (rec x nil)))

(defun group (source n)
  "This takes a  flat list and emit a list of lists, each n long
   containing the elements of the original list"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n)
				   acc))
		   (nreverse (cons source acc))))))
    (if source 
	(rec source nil) 
	nil)))

(defun last1 (list)
  (car (last list)))

(defun starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))

(defun find-anywhere (item tree)
    "Does item occur anywhere in tree?"
    (if (atom tree)
	(if (eql item tree) tree)
	(or (find-anywhere item (first tree))
	    (find-anywhere item (rest tree)))))

(defun unique-find-if-anywhere (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-if-anywhere
        predicate
        (first tree)
        (unique-find-if-anywhere predicate (rest tree)
                                 found-so-far))))

(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun listify (x) (if (listp x) x (list x)))

