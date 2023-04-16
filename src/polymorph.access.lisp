;;;; polymorph.access.lisp

(in-package #:polymorph.access)


;;; At
(define-polymorphic-function at (container &rest keys) :overwrite t
  :documentation "Return the element of the container specified by the keys.")
(define-polymorphic-function (setf at) (new container &rest keys) :overwrite t
  :documentation "Setf the element of the container, specified by the keys, to new.")
(define-polymorphic-function at-safe (container &rest keys) :overwrite t
  :documentation "Return the element of the container specified by the keys.")
(define-polymorphic-function (setf at-safe) (new container &rest keys) :overwrite t
  :documentation "Setf the element of the container, specified by the keys, to new.")


(defpolymorph (at :inline t) ((array array) &rest indexes) (values t &optional)
  (apply #'aref array indexes))

(defpolymorph (at-safe :inline t) ((array array) &rest indexes) (values t boolean &optional)
  (if (apply #'array-in-bounds-p array indexes)
      (values (apply #'aref array indexes) t)
      (values nil nil)))


(defpolymorph-compiler-macro at (array &rest) (&whole form array &rest indexes &environment env)
  (with-type-info (_ (array-type &optional elt-type) env) array
    (when-types ((array-type array)) form
      (if (constantp (length indexes) env)
          `(the (values ,elt-type &optional) (aref ,array ,@indexes))
          `(the (values ,elt-type &optional) ,form)))))

(defpolymorph-compiler-macro at-safe (array &rest) (&whole form array &rest indexes &environment env)
  (with-type-info (_ (array-type &optional elt-type) env) array
    (when-types ((array-type array)) form
      (if (constantp (length indexes) env)
          `(the (values (or ,elt-type null) boolean &optional)
                (if (array-in-bounds-p ,array ,@indexes)
                    (values (aref ,array ,@indexes) t)
                    (values nil nil)))
          `(the (values (or ,elt-type null) &optional boolean) ,form)))))



(defpolymorph ((setf at) :inline t) ((new t) (array array) &rest indexes) (values t &optional)
  (let ((new-type (type-of new)))
    (if (not (subtypep new-type (array-element-type array)))
        (error 'type-error :expected-type (array-element-type array) :datum new)
        (setf (apply #'aref array indexes) new))))

(defpolymorph ((setf at-safe) :inline t) ((new t) (array array) &rest indexes) (values t boolean &optional)
  (let ((new-type (type-of new)))
    (if (not (subtypep new-type (array-element-type array)))
        (error 'type-error :expected-type (array-element-type array) :datum new)
        (if (apply #'array-in-bounds-p array indexes)
            (values (setf (apply #'aref array indexes) new) t)
            (values nil nil)))))


(defpolymorph-compiler-macro (setf at) (t array &rest) (&whole form
                                                               new array &rest indexes
                                                               &environment env)
  (with-type-info (_ (array-type &optional elt-type) env) array
    (when-types ((array-type array)) form
      (let ((new-type (with-type-info (type () env) new type)))
        (cond ((not (subtypep new-type elt-type env))
               (error 'type-error :expected-type elt-type :datum new))

              ((constantp (length indexes) env)
               `(the (values ,new-type &optional)
                    (funcall #'(setf aref) ,new ,array ,@indexes)))
              (t
               `(the (values new-type &optional) ,form)))))))

(defpolymorph-compiler-macro (setf at-safe) (t array &rest) (&whole form
                                                                    new array &rest indexes
                                                                    &environment env)
  (with-type-info (_ (array-type &optional elt-type) env) array
    (when-types ((array-type array)) form
      (let ((new-type (with-type-info (type () env) new type)))
        (cond ((not (subtypep new-type elt-type env))
               (error 'type-error :expected-type elt-type :datum new))

             ((constantp (length indexes) env)
              `(if (array-in-bounds-p ,array ,@indexes)
                   (values (the (values ,new-type &optional)
                                (funcall #'(setf aref) ,new ,array ,@indexes))
                           t)
                   (values nil nil)))
             (t
              `(the (values (or ,new-type null) boolean &optional) ,form)))))))




(defpolymorph (at :inline t) ((list list) (index ind))
    (values t &optional)
  (let* ((list (nthcdr index list)))
     (if list
         (first list)
         (error 'simple-error :format-control "Index not in list bounds"))))


(defpolymorph (at-safe :inline t) ((list list) (index ind))
    (values t boolean &optional)
  (let* ((list (nthcdr index list)))
    (if list
        (values (first list) t)
        (values nil nil))))



(defpolymorph ((setf at) :inline t) ((new t) (list list) (index ind))
    (values t &optional)
  (let* ((list (nthcdr index list)))
    (if list
        (setf (first list) new)
        (error 'simple-error :format-control "Index not in list bounds"))))


(defpolymorph ((setf at-safe) :inline t) ((new t) (list list) (index ind))
    (values t &optional boolean)
  (let* ((list (nthcdr index list)))
    (if list
        (values (setf (first list) new) t)
        (values nil nil))))



(defpolymorph (at :inline t) ((ht hash-table) key) (values t &optional)
  (multiple-value-bind (res ok) (gethash key ht)
    (if ok
        res
        (error 'simple-error :format-control "Key not found"))))

(defpolymorph (at-safe :inline t) ((ht hash-table) key) (values t boolean &optional)
  (gethash key ht))



(defpolymorph ((setf at) :inline t) ((new t) (ht hash-table) key) (values t &optional)
  (multiple-value-bind (_ ok) (gethash key ht)
    (declare (ignore _))
    (if ok
        (setf (gethash key ht) new)
        (error 'simple-error :format-control "Key not found"))))

(defpolymorph ((setf at-safe) :inline t) ((new t) (ht hash-table) key) (values t boolean &optional)
  (values (setf (gethash key ht) new) t))



(define-setf-expander at (container &rest indexes &environment env)
  (with-type-info (container-type () env) container
    (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion container env)
      (declare (ignorable dummies vals getter setter))
      (let ((dummy (gensym)))
        (values `(,dummy)
                (list container)
                newval
                `(funcall #'(setf at) ,@newval
                         (the ,container-type ,dummy) ,@indexes)
                `(at (the ,container-type ,dummy) ,@indexes))))))

(define-setf-expander at-safe (container &rest indexes &environment env)
  (with-type-info (container-type () env) container
    (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion container env)
      (declare (ignorable dummies vals getter setter))
      (let ((dummy (gensym)))
        (values `(,dummy)
                (list container)
                newval
                `(funcall #'(setf at-safe) ,@newval
                         (the ,container-type ,dummy) ,@indexes)
                `(at-safe (the ,container-type ,dummy) ,@indexes))))))


;; TODO Do I even need this?
(define-polymorphic-function row-major-at (container key) :overwrite t
  :documentation "Typed row-major-aref.")
(define-polymorphic-function (setf row-major-at) (new container key) :overwrite t
  :documentation "Setf for row-majpr-at.")

(defpolymorph (row-major-at :inline t) ((array array) (index ind)) t
   (row-major-aref array index))

(defpolymorph-compiler-macro row-major-at (array ind) (&whole form array index &environment env)
  (with-type-info (_ (array-type &optional elt-type) env) array
    (when-types ((array-type array)) form
      `(the ,elt-type (cl:row-major-aref ,array ,index)))))


;;;Front/Back
(define-polymorphic-function back (container) :overwrite t
  :documentation "Return last element of the container.")
(define-polymorphic-function front (container) :overwrite t
  :documentation "Return first of the container.")
(define-polymorphic-function (setf front) (new container) :overwrite t
  :documentation "Setf the first element of the containter to new.")
(define-polymorphic-function (setf back) (new container) :overwrite t
  :documentation "Setf the last element of the container to new.")
(define-polymorphic-function back-safe (container) :overwrite t
  :documentation "Return last element of the container.")
(define-polymorphic-function front-safe (container) :overwrite t
  :documentation "Return first of the container.")
(define-polymorphic-function (setf front-safe) (new container) :overwrite t
  :documentation "Setf the first element of the containter to new.")
(define-polymorphic-function (setf back-safe) (new container) :overwrite t
  :documentation "Setf the last element of the container to new.")


(defpolymorph (front :inline t) ((container list)) (values t &optional)
  (if container
      (first container)
      (error 'simple-error :format-control "List is empty")))

(defpolymorph (front-safe :inline t) ((container list)) (values t boolean &optional)
  (if container
      (values (first container) t)
      (values nil nil)))

(defpolymorph ((setf front) :inline t) ((new t) (container list)) (values t &optional)
  (if container
      (setf (first container) new)
      (error 'simple-error :format-control "List is empty")))

(defpolymorph ((setf front-safe) :inline t) ((new t) (container list)) (values t boolean &optional)
 (if container
     (values (setf (first container) new) t)
     (values nil nil)))

(defpolymorph (back :inline t) ((container list)) (values t &optional)
  (if container
      (first (last container))
      (error 'simple-error :format-control "List is empty")))

(defpolymorph (back-safe :inline t) ((container list)) (values t boolean &optional)
  (if container
      (values (first (last container)) t)
      (values nil nil)))

(defpolymorph ((setf back) :inline t) ((new t) (container list)) (values t &optional)
  (if container
      (setf (first (last container)) new)
      (error 'simple-error :format-control "List is empty")))

(defpolymorph ((setf back) :inline t) ((new t) (container list)) (values t boolean &optional)
  (if container
      (values (setf (first (last container)) new) t)
      (values nil nil)))

(defpolymorph (front :inline t) ((container array)) (values t &optional)
   (assert (= 1 (array-rank container)))
   (aref container 0))

(defpolymorph (front-safe :inline t) ((container array)) (values t boolean &optional)
  (assert (= 1 (array-rank container)))
  (if (= 0 (length container))
      (values nil nil)
      (values (aref container 0) t)))

(defpolymorph-compiler-macro front (array)
    (&whole form container &environment env)
  (with-type-info (_ (array-type &optional elt-type dim) env) container
    (when-types ((array-type array)) form
      (cond ((eql dim 'cl:*)
             (warn "An array should be of rank 1"))
            ((< 1 (length dim))
             (error "An array should be of rank 1"))) ;;FIXME this doesn't trigger
      `(the (values ,elt-type &optional)
            (aref ,container 0)))))

(defpolymorph-compiler-macro front-safe (array)
    (&whole form container &environment env)
  (with-type-info (_ (array-type &optional elt-type dim) env) container
    (when-types ((array-type array)) form
      (cond ((eql dim 'cl:*)
             (warn "An array should be of rank 1"))
            ((< 1 (length dim))
             (error "An array should be of rank 1"))) ;;FIXME this doesn't trigger
      `(the (values (or ,elt-type null) boolean &optional)
            ,(once-only (container)
               `(if (= 0 (length ,container))
                    (values nil nil)
                    (values (aref ,container 0) t)))))))

(defpolymorph ((setf front) :inline t) ((new t) (container array)) (values t &optional)
  (assert (= 1 (array-rank container)))
  (setf (aref container 0) new))


(defpolymorph ((setf front-safe) :inline t) ((new t) (container array)) (values t boolean &optional)
  (assert (= 1 (array-rank container)))
  (if (= 0 (length container))
      (values nil nil)
      (values (setf (aref container 0) new) t)))


(defpolymorph-compiler-macro (setf front) (t array)
    (&whole form new container &environment env)
  (with-type-info (_ (array-type &optional elt-type dim) env) container
    (when-types ((array-type array)) form
      (let ((new-type (with-type-info (type () env) new type)))
        (cond ((not (subtypep new-type elt-type env))
               (error 'type-error :expected-type elt-type :datum new))
              ((eql dim 'cl:*)
               (warn "An array should be of rank 1"))
              ((< 1 (length dim))
               (error "An array should be of rank 1"))) ;;FIXME this doesn't trigger
        `(the (values ,new-type &optional)
              (setf (aref ,container 0) ,new))))))


(defpolymorph-compiler-macro (setf front) (t array)
    (&whole form new container &environment env)
  (with-type-info (_ (array-type &optional elt-type dim) env) container
    (when-types ((array-type array)) form
      (let ((new-type (with-type-info (type () env) new type)))
        (cond ((not (subtypep new-type elt-type env))
               (error 'type-error :expected-type elt-type :datum new))
              ((eql dim 'cl:*)
               (warn "An array should be of rank 1"))
              ((< 1 (length dim))
               (error "An array should be of rank 1"))) ;;FIXME this doesn't trigger
        `(the (values (or ,new-type null) boolean &optional)
              ,(once-only (container)
                 `(if (= 0 (length ,container))
                      (values nil nil)
                      (values (setf (aref ,container 0) ,new) t))))))))


(defpolymorph (back :inline t) ((container array)) (values t &optional)
   (assert (= 1 (array-rank container)))
   (aref container (- (length container) 1)))

(defpolymorph (back-safe :inline t) ((container array)) (values t boolean &optional)
  (assert (= 1 (array-rank container)))
  (let ((l (length container)))
    (if (= 0 l)
        (values nil nil)
        (values (aref container (- l 1)) t))))

(defpolymorph-compiler-macro back (array)
    (&whole form container &environment env)
  (with-type-info (_ (array-type &optional elt-type dim) env) container
    (when-types ((array-type array)) form
      (cond ((eql dim 'cl:*)
             (warn "An array should be of rank 1"))
            ((< 1 (length dim))
             (error "An array should be of rank 1"))) ;;FIXME this doesn't trigger
      (once-only (container)
        `(the (values ,elt-type &optional)
          (aref ,container (- (length ,container) 1)))))))

(defpolymorph-compiler-macro back-safe (array)
    (&whole form container &environment env)
  (with-type-info (_ (array-type &optional elt-type dim) env) container
    (when-types ((array-type array)) form
      (cond ((eql dim 'cl:*)
             (warn "An array should be of rank 1"))
            ((< 1 (length dim))
             (error "An array should be of rank 1"))) ;;FIXME this doesn't trigger
      (let ((l (gensym "L")))
        `(the (values (or ,elt-type null) boolean &optional)
          ,(once-only (container)
             `(let ((,l (length ,container)))
                (if (= 0 ,l)
                    (values nil nil)
                    (values (aref ,container (- ,l 1)) t)))))))))

(defpolymorph ((setf back) :inline t) ((new t) (container array)) (values t &optional)
  (assert (= 1 (array-rank container)))
  (setf (aref container (- (length container) 1)) new))


(defpolymorph ((setf back-safe) :inline t) ((new t) (container array)) (values t boolean &optional)
  (assert (= 1 (array-rank container)))
  (let ((l (length container)))
    (if (= 0 l)
        (values nil nil)
        (values (setf (aref container (- l 1)) new) t))))


(defpolymorph-compiler-macro (setf back) (t array)
    (&whole form new container &environment env)
  (with-type-info (_ (array-type &optional elt-type dim) env) container
    (when-types ((array-type array)) form
      (let ((new-type (with-type-info (type () env) new type)))
        (cond ((not (subtypep new-type elt-type env))
               (error 'type-error :expected-type elt-type :datum new))
              ((eql dim 'cl:*)
               (warn "An array should be of rank 1"))
              ((< 1 (length dim))
               (error "An array should be of rank 1"))) ;;FIXME this doesn't trigger
        `(the (values ,new-type &optional)
          ,(once-only (container)
             `(setf (aref ,container (- (length ,container) 1)) ,new)))))))


(defpolymorph-compiler-macro (setf back-safe) (t array)
    (&whole form new container &environment env)
  (with-type-info (_ (array-type &optional elt-type dim) env) container
    (when-types ((array-type array)) form
      (let ((new-type (with-type-info (type () env) new type)))
        (cond ((not (subtypep new-type elt-type env))
               (error 'type-error :expected-type elt-type :datum new))
              ((eql dim 'cl:*)
               (warn "An array should be of rank 1"))
              ((< 1 (length dim))
               (error "An array should be of rank 1"))) ;;FIXME this doesn't trigger
        (let ((l (gensym "L")))
          `(the (values (or ,new-type null) boolean &optional)
            ,(once-only (container)
               `(let ((,l (length ,container)))
                  (if (= 0 ,l)
                      (values nil nil)
                      (values (setf (aref ,container (- ,l 1)) ,new) t))))))))))



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


(defpolymorph size ((object (and array (not vector) (not bit-vector)))) (values ind &optional)
  (cl:array-total-size object))

(defpolymorph size ((object (or vector bit-vector string))) (values ind &optional)
  (cl:length object))


(defpolymorph capacity ((object array)) (values ind &optional)
  (cl:array-total-size object))

(defpolymorph size ((object list)) (values (or null ind) &optional)
  (list-length object))

(defpolymorph capacity ((object list)) (values (or null ind) &optional)
  (list-length object))

(defpolymorph size ((object hash-table)) (values ind &optional)
  (hash-table-count object))

(defpolymorph capacity ((object hash-table)) (values ind &optional)
  (hash-table-size object))
