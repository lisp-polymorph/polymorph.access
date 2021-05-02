;;;; polymorph.access.lisp

(in-package #:polymorph.access)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %form-type (form &optional env)
    (if (constantp form env)
        (let ((val (eval form))) ;;need custom eval that defaults to sb-ext:eval-in-lexenv here)
          (if (typep val '(or number character symbol))
              (values `(eql ,val) t)
              (values (type-of val) t)))
        (adhoc-polymorphic-functions::primary-form-type form env)))

  (deftype ind () `(integer 0 #.array-dimension-limit)))

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
  (let* ((ar-type (%form-type array env))
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
      `(nth ,@indexes ,list)
      `(apply #'nth ,indexes ,list)))



(defpolymorph (setf at) ((new t) (list list) &rest indexes) t
  (setf (apply #'elt list indexes) new))



(defpolymorph-compiler-macro (setf at) (t list &rest) (new list &rest indexes &environment env)
  (if (constantp (length indexes) env)
      `(setf (nth ,@indexes ,list) ,new)
      `(setf (apply #'nth ,indexes ,list) new)))





(defpolymorph at ((ht hash-table) &rest indexes) (values t boolean &optional)
  (apply #'gethash (first indexes) ht (cdr indexes)))



(defpolymorph-compiler-macro at (hash-table &rest) (&whole form ht &rest indexes &environment env)
    (if (constantp (length indexes) env)
        (once-only (indexes)
          `(gethash ,(first indexes) ,ht ,(second indexes)))
        form))


(defpolymorph (setf at) ((new t) (ht hash-table) &rest indexes) t
  (setf (apply #'gethash (first indexes) ht (cdr indexes)) new))

(defpolymorph-compiler-macro (setf at) (t hash-table &rest) (&whole form
                                                                    new ht &rest indexes
                                                                    &environment env)
  (if (constantp (length indexes) env)
      (once-only (indexes)
        `(setf (gethash ,(first indexes) ,ht ,(second indexes)) ,new))
      form))


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
  (let* ((type (%form-type container env))
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
  (let* ((type (%form-type container env))
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




;;;Emptyp
(define-polymorphic-function emptyp (container) :overwrite t
  :documentation "Return T if container is empty, and NIL otherwise.")

(defpolymorph emptyp ((object vector)) (values boolean &optional)
              (= 0 (cl:length object)))


(defpolymorph emptyp ((object list)) (values boolean &optional)
              (null object))


(defpolymorph emptyp ((object hash-table)) (values boolean &optional)
              (= 0 (hash-table-count object)))




;;; Size
(define-polymorphic-function size (continer) :overwrite t
  :documentation "Return the size of stored data inside the container.")
(define-polymorphic-function capacity (container) :overwrite t
  :documentation "Return the upper limit of what can be currently
  stored in the container.")
;; TODO Should emptyp use it? Maybe

(defpolymorph size ((object array)) (values ind &optional)
  (typecase object
    ((or vector bit-vector string) (cl:length object))
    (otherwise (cl:array-total-size object))))

(defpolymorph-compiler-macro size (array) (object &environment env)
  (let* ((type (%form-type object env)))
    (cond ((subtypep type '(or vector bit-vector string) env)
           `(length ,object))    ;; TODO this can probably be improved/less ugly
          (t `(cl:array-total-size ,object)))))


(defpolymorph capacity ((object array)) (values ind &optional)
  (cl:array-total-size object))

(defpolymorph size ((object list)) (values ind &optional)
  (length object))

(defpolymorph capacity ((object list)) (values ind &optional)
  (length object))


(defpolymorph size ((object hash-table)) (values ind &optional)
  (hash-table-count object))

(defpolymorph capacity ((object hash-table)) (values ind &optional)
  (hash-table-size object))


