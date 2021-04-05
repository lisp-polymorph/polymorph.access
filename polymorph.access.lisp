;;;; polymorph.access.lisp

(in-package #:polymorph.access)


(defun %form-type (form &optional env)
  (adhoc-polymorphic-functions::form-type form env))



;;; At
(define-polymorphic-function at (container &rest keys) :overwrite t
  :documentation "If used with array/list return the element of the container specified by the keys.
  If used with a map, find the entry in it with (funcall test key KEY) => T and return the
  associated value and T as multiple values, or return DEFAULT and NIL if there is no such entry.")
(define-polymorphic-function (setf at) (new container &rest keys) :overwrite t
  :documentation "Setf for at. Use return value of at if needed.")

(defpolymorph at ((array array) &rest indexes) t
  (apply #'aref array indexes))

(defpolymorph-compiler-macro at (array &rest) (array &rest indexes &environment env)
  (let* ((ar-type (%form-type array env))
         (elt-type  (cm:array-type-element-type ar-type)))
    (if (constantp (length indexes) env)
        `(the (values ,elt-type &optional) (aref ,array ,@indexes))
        `(the (values ,elt-type &optional) (apply #'aref ,array ,indexes)))))


(defpolymorph (setf at) ((new t) (array array) &rest indexes) t
  (setf (apply #'aref array indexes) new))



(defpolymorph-compiler-macro (setf at) (t array &rest) (new array &rest indexes &environment env)
  (let* ((ar-type (cm:form-type array env))
         (elt-type  (cm:array-type-element-type ar-type))
         (new-type (cm:form-type new env)))
    (cond ((not (subtypep new-type elt-type env))
           (error 'type-error :expected-type elt-type :datum new))
          ((constantp (length indexes) env)
           `(the (values ,elt-type &optional) (setf (aref ,array ,@indexes)
                                                    (the (values ,elt-type &optional) ,new))))
          (t
           `(the (values ,elt-type &optional) (setf (apply #'aref ,array ,indexes)
                                                    (the (values ,elt-type &optional) ,new)))))))



(defpolymorph at ((list list) &rest indexes) t
  (apply #'elt (the cons list) indexes))



(defpolymorph-compiler-macro at (list &rest) (list &rest indexes &environment env)
  (if (constantp (length indexes) env)
      `(elt ,list ,@indexes)
      `(apply #'elt ,list ,indexes)))



(defpolymorph (setf at) ((new t) (list list) &rest indexes) t
  (setf (apply #'elt list indexes) new))



(defpolymorph-compiler-macro (setf at) (t list &rest) (new list &rest indexes &environment env)
  (if (constantp (length indexes) env)
      `(setf (elt ,list ,@indexes) ,new)
      `(setf (apply #'elt ,list ,indexes) new)))





(defpolymorph at ((ht hash-table) &rest indexes) (values t boolean &optional)
  (apply #'gethash (first indexes) ht (cdr indexes)))



(defpolymorph-compiler-macro at (hash-table &rest) (ht &rest indexes &environment env)
  (let ((ht-type (print (cm:form-type ht env))))
    (if (constantp (length indexes) env)
        (if (listp ht-type) ;; TODO this trick doesn't work unfortunately, but it might.
            (let ((key-type (second ht-type))
                  (val-type (third ht-type))
                  (attempt-type (cm:form-type (car indexes) env)))
              (if (subtypep attempt-type key-type env)
                  `(the (values ,val-type boolean &optional)
                        (gethash ,(first indexes) ,ht ,(second indexes)))
                  (error 'type-error :expected-type key-type :datum (car indexes))))
            `(gethash ,(first indexes) ,ht ,(second indexes)))
        (once-only (indexes)
          `(apply #'gethash (first ,indexes) ,ht (cdr ,indexes))))))



(defpolymorph (setf at) ((new t) (ht hash-table) &rest indexes) t
  (setf (apply #'gethash (first indexes) ht (cdr indexes)) new))


;;;Front/Back
(define-polymorphic-function back (container) :overwrite t
  :documentation "Return last element of the container.")
(define-polymorphic-function front (container) :overwrite t
  :documentation "Return first of the container.")
(define-polymorphic-function (setf front) (new container) :overwrite t
  :documentation "Setf for front. Use return value of front if needed.")
(define-polymorphic-function (setf back) (new container) :overwrite t
  :documentation "Setf for back. Use return value of back if needed.")

(defpolymorph front ((container list)) t
  (first container))


(defpolymorph (setf front) ((new t) (container list)) t
  (setf (first container) new))

(defpolymorph back ((container list)) t
  (car (last container)))

(defpolymorph (setf back) ((new t) (container list)) t
  (setf (first (last container)) new))


(defpolymorph front ((container array)) t
  (assert (= 1 (array-rank container)))
  (aref container 0))

(defpolymorph-compiler-macro front (array) (container &environment env)
  (let* ((type (cm:form-type container env))
         (elt-type  (cm:array-type-element-type type))
         (dim  (cm:array-type-dimensions type)))
    `(the (values ,elt-type &optional)
          (progn
            ,(cond ((eql dim 'cl:*)
                    (warn "An array should be of rank 1"))
                   ((> 1 (length dim))
                    (error "An array should be of rank 1")) ;;FIXME this doesn't trigger
                   (t t)) ;;instead sbcl check the aref against dimensions
            (aref ,container 0)))))                         ;; Not great, not terrible

(defpolymorph (setf front) ((new t) (container array)) t
  (setf (aref container 0) new))



(defpolymorph back ((container array)) t
              (assert (= 1 (array-rank container)))
              (aref container (1- (length container))))

(defpolymorph-compiler-macro back (array) (container &environment env)
  (let* ((type (cm:form-type container env))
         (elt-type  (cm:array-type-element-type type))
         (dim  (cm:array-type-dimensions type)))
    `(the (values,elt-type &optional)
          (progn
            ,(cond ((eql dim 'cl:*)
                    (warn "An array should be of rank 1"))
                   ((> 1 (length dim))
                    (error "An array should be of rank 1"))
                   (t t))
            ,(once-only (container)
                        `(aref ,container (1- (length ,container))))))))
