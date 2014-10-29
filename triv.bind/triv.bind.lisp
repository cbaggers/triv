;;;; triv.bind.lisp

(in-package #:triv.bind)

(defmacro dbind (lambda-list expressions &body body)
  `(destructuring-bind ,lambda-list ,expressions ,@body))

(defmacro dmap (bindings expression &body body)
  "(d-map ((x) y z) '((1 2 3) (4 5 6) (7 8 9))
    (print x)
    (print y)
    (print z))"
  (let* ((appends (mapcar #'listp bindings))
         (bindings (mapcar (lambda (b) (if (listp b) (first b) b))
                           (break "" bindings)))
         (syms (mapcar (lambda (x) (gensym (symbol-name x))) bindings)))
    `(destructuring-bind ,bindings
         (loop :for ,syms :in ,expression 
            ,@(mapcan (lambda (b s a) `(,(if a :append :collect) ,s :into ,b)) 
                      bindings syms appends)
            :finally (return ,(cons 'list bindings)))
       ,@body)))

(defmacro if-let (bindings &body (then-form &optional else-form))
    "Creates new variable bindings, and conditionally executes either
THEN-FORM or ELSE-FORM. ELSE-FORM defaults to NIL.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, the THEN-FORM is executed with the
bindings in effect, otherwise the ELSE-FORM is executed with the bindings in
effect."
    (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                             (list bindings)
                             bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           ,then-form
           ,else-form))))

(defmacro when-let (bindings &body forms)
    "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

All initial-forms are executed sequentially in the specified order. Then all
the variables are bound to the corresponding values.

If all variables were bound to true values, then FORMS are executed as an
implicit PROGN."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@forms))))

(defmacro when-let* (bindings &body forms)
  "Creates new variable bindings, and conditionally executes FORMS.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

Each initial-form is executed in turn, and the variable bound to the
corresponding value. Initial-form expressions can refer to variables
previously bound by the WHEN-LET*.

Execution of WHEN-LET* stops immediately if any initial-form evaluates to NIL.
If all initial-forms evaluate to true, then FORMS are executed as an implicit
PROGN."
  (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                          (list bindings)
                          bindings)))
    (labels ((bind (bindings forms)
               (if bindings
                   `((let (,(car bindings))
                       (when ,(caar bindings)
                         ,@(bind (cdr bindings) forms))))
                   forms)))
      `(let (,(car binding-list))
         (when ,(caar binding-list)
           ,@(bind (cdr binding-list) forms))))))


(defun lambda-list-split (template lam-list)
  (labels ((kwd (x) (intern (format nil "~a" x) :keyword))
           (collector (lam-list &optional current-modifier accum)
                (let ((item (first lam-list)))
                  (cond ((null lam-list) accum) 
                        ((and (symbolp item) (eql (elt (symbol-name item) 0) #\&))
                         (collector (rest lam-list)
                                    (kwd item)
                                    accum))
                        (t (collector (rest lam-list)
                                      current-modifier
                                      (acons current-modifier 
                                             (cons item 
                                                   (cdr (assoc current-modifier
                                                               accum)))
                                             accum))))))
           (clean-alist (alist &optional accum)
             (let ((item (first alist)))
               (cond ((null alist) accum)
                     ((atom item) (clean-alist (rest alist) accum))
                     ((not (assoc (first item) accum))
                      (clean-alist (rest alist) (cons item accum)))
                     (t (clean-alist (rest alist) accum)))))
           (first-in-template (x) (member (first x) template)))
    (let ((template (when template (cons nil (mapcar #'kwd template))))
          (split (collector lam-list)))
      (if (or (null template)
              (every #'first-in-template split))
          (clean-alist split)
          (error "&symbol found that was not specified in template ~s" 
                 (mapcar #'first split))))))

;;------------------------------------------------------------
;; CREDITS - Concept and fist impl McQuade, current impl triv

(defun letf-expander (bindings body)
  (let ((e (first bindings)))
    (cond ((null bindings) body)
          ((atom e) `((let ((,e nil))
                         ,@(letf-expander (rest bindings) body))))
          
          (t (let ((id (and (consp (car e)) (caar e))))
               (if (eql id 'function)
                   `((labels ((,(cadar e) ,@(cdr e)))
                        ,@(letf-expander (rest bindings) body)))
                   `((let (,e)
                        ,@(letf-expander (rest bindings) body)))))))))

(defmacro letf (bindings &body body)
  "An extended let form which combines labels and let"
  (first (letf-expander bindings body)))

;; Example:
;; (letf ((#'test (x) (format t \"woo ~a~%\" x))
;;        (b 1)
;;        (c (+ 1 2)))
;;   (test \"oh\")
;;   (test (+ b c)))
;;------------------------------------------------------------
