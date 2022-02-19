;;;; polymorph.access.lisp

(in-package #:polymorph.access)



;;; At
(define-polymorphic-function at (container &rest keys) :overwrite t
  :documentation "Return the element of the container specified by the keys.")
(define-polymorphic-function (setf at) (new container &rest keys) :overwrite t
  :documentation "Setf the element of the container, specified by the keys, to new.")


(defpolymorph (at :inline t) ((array array) &rest indexes) (values t &optional boolean)
  (let ((error-policy (member :error indexes)))
    (if error-policy

        (let ((indexes (butlast (butlast indexes))))
          (if (second error-policy)
              (apply #'aref array indexes)
              (if (apply #'array-in-bounds-p array indexes)
                  (values (apply #'aref array indexes) t)
                  (values nil nil))))

        (apply #'aref array indexes))))




(defpolymorph-compiler-macro at (array &rest) (&whole form array &rest indexes &environment env)
  (with-type-info (_ (array-type &optional elt-type) env) array
    (when-types ((array-type array)) form
      (if (constantp (length indexes) env)
          (let ((error-policy (member :error indexes)))

            (if error-policy
                (let ((indexes (butlast (butlast indexes))))
                  (if (second error-policy)
                      `(the (values ,elt-type &optional)
                            (aref ,array ,@indexes))
                      `(the (values (or ,elt-type null) boolean &optional)
                            (if (array-in-bounds-p ,array ,@indexes)
                                (values (aref ,array ,@indexes) t)
                                (values nil nil)))))
                `(the (values ,elt-type &optional) (aref ,array ,@indexes))))

          `(the (values (or ,elt-type null) &optional boolean) ,form)))))


(defpolymorph ((setf at) :inline t) ((new t) (array array) &rest indexes) (values t &optional boolean)
 (let ((error-policy (member :error indexes)))
    (if error-policy

        (let ((indexes (butlast (butlast indexes))))
          (if (second error-policy)
              (setf (apply #'aref array indexes) new)
              (if (apply #'array-in-bounds-p array indexes)
                  (values (setf (apply #'aref array indexes) new) t)
                  (values nil nil))))

        (setf (apply #'aref array indexes) new))))



(defpolymorph-compiler-macro (setf at) (t array &rest) (&whole form
                                                               new array &rest indexes
                                                               &environment env)
  (with-type-info (_ (array-type &optional elt-type) env) array
    (when-types ((array-type array)) form
      (let ((new-type (with-type-info (type () env) new type)))
        (cond ((not (subtypep new-type elt-type env))
               (error 'type-error :expected-type elt-type :datum new))

              ((constantp (length indexes) env)
               (let ((error-policy (member :error indexes)))
                 (if error-policy
                     (let ((indexes (butlast (butlast indexes))))
                       (if (second error-policy)
                           `(the (values ,elt-type &optional)
                                 (funcall #'(setf aref) ,new ,array ,@indexes))
                           `(the (values (or ,elt-type null) boolean &optional)
                                 (if (array-in-bounds-p ,array ,@indexes)
                                     (values (funcall #'(setf aref) ,new ,array ,@indexes) t)
                                     (values nil nil)))))
                     `(the (values ,elt-type &optional)
                           (funcall #'(setf aref) ,new ,array ,@indexes)))))

              (t
               `(the (values (or ,elt-type null) &optional boolean) ,form)))))))




(defpolymorph at ((list list) (index ind) &key ((error boolean) t))
    (values t &optional boolean)
  (let* ((list (nthcdr index list)))
    (if error
        (if list
            (first list)
            (error 'simple-error :format-control "Index not in list bounds"))
        (if list
            (values (first list) t)
            (values nil nil)))))


#||
(defpolymorph-compiler-macro at (list ind) (&whole form list ind &environment env)
  (let ((error-policy (member :error indexes)))
    (let ((index (gensym "I"))
          (listname (gensym "LIST")))
      (if error-policy

          `(let* ((,index ,ind)
                  (,listname (nthcdr ,index ,list)))
             (if ,(second error-policy)
                 (if ,listname
                     (first ,listname)
                     (error 'simple-error :format-control "Index not in list bounds"))
                 (if ,listname
                     (values (first ,listname) t)
                     (values nil nil))))

          `(let* ((,index ,ind)
                  (,listname (nthcdr ,index ,list)))
             (if ,listname
                 (first ,listname)
                 (error 'simple-error :format-control "Index not in list bounds")))))))
||#


(defpolymorph (setf at) ((new t) (list list) (index ind) &key ((error boolean) t))
    (values t &optional boolean)
  (let* ((list (nthcdr index list)))
    (if error
        (if list
            (setf (first list) new)
            (error 'simple-error :format-control "Index not in list bounds"))
        (if list
            (values (setf (first list) new) t)
            (values nil nil)))))

#||
(defpolymorph-compiler-macro (setf at) (t list ind) (&whole form
                                                            new list ind
                                                            &environment env)
  (let ((error-policy (member :error indexes)))
    (let ((index (gensym "I"))
          (listname (gensym "LIST")))
      (if error-policy

          `(let* ((,index ,ind)
                  (,listname (nthcdr ,index ,list)))
             (if ,(second error-policy)
                 (if ,listname
                     (setf (first ,listname) ,new)
                     (error 'simple-error :format-control "Index not in list bounds"))
                 (if ,listname
                     (values (setf (first ,listname) ,new) t)
                     (values nil nil))))

          `(let* ((,index ,ind)
                  (,listname (nthcdr ,index ,list)))
             (if ,listname
                 (setf (first ,listname) ,new)
                 (error 'simple-error :format-control "Index not in list bounds")))))))
||#



(defpolymorph at ((ht hash-table) &rest indexes) (values t &optional boolean)
  (let ((error-policy (member :error indexes)))
    (if error-policy

        (let ((index (first indexes)))
          (if (second error-policy)
              (multiple-value-bind (res ok) (gethash index ht)
                (if ok
                    res
                    (error 'simple-error :format-control "Key not found")))
              (gethash index ht)))

        (let ((index (first indexes)))
          (gethash index ht)))))




(defpolymorph-compiler-macro at (hash-table &rest) (&whole form ht &rest indexes &environment env)
  (if (constantp (length indexes) env)
      (let ((error-policy (member :error indexes)))
        (let ((index (gensym "I")) (res (gensym "RES")) (ok (gensym "OK")))
          (if error-policy

              `(let ((,index (first ,indexes)))
                 (if ,(second error-policy)
                     (multiple-value-bind (,res ,ok) (gethash ,index ,ht)
                       (if ,ok
                           ,res
                           (error 'simple-error :format-control "Key not found")))
                     (gethash ,index ,ht)))

              `(let ((,index (first ,indexes)))
                 (gethash ,index ,ht)))))

      form))

(defpolymorph (setf at) ((new t) (ht hash-table) &rest indexes) (values t &optional boolean)
  (let ((error-policy (member :error indexes)))
    (if error-policy

        (let ((index (first indexes)))
          (if (second error-policy)
              (multiple-value-bind (_ ok) (gethash index ht)
                (declare (ignore _))
                (if ok
                    (setf (gethash index ht) new)
                    (error 'simple-error :format-control "Key not found")))
              (values (setf (gethash index ht) new) t)))

        (let ((index (first indexes)))
          (values (setf (gethash index ht) new) t)))))


(defpolymorph-compiler-macro (setf at) (t hash-table &rest) (&whole form
                                                                    new ht &rest indexes
                                                                    &environment env)
  (if (constantp (length indexes) env)
      (let ((error-policy (member :error indexes)))
        (let ((index (gensym "I")) (_ (gensym "_")) (ok (gensym "OK")))
          (if error-policy

              `(let ((,index (first ,indexes)))
                 (if ,(second error-policy)
                     (multiple-value-bind (,_ ,ok) (gethash ,index ,ht)
                       (declare (ignore ,_))
                       (if ,ok
                           (setf (gethash ,index ,ht) ,new)
                           (error 'simple-error :format-control "Key not found")))
                     (values (setf (gethash ,index ,ht) ,new) t)))

              `(let ((,index (first ,indexes)))
                 (values (setf (gethash ,index ,ht) ,new) t)))))

      form))


(define-setf-expander at (container &rest indexes &environment env)
  (with-type-info (container-type () env) container
    (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion container env)
      (declare (ignorable setter))
      (values dummies
              vals
              newval
              `(funcall #'(setf at) ,@newval
                        (the ,container-type ,getter) ,@indexes)
              `(at (the ,container-type ,getter) ,@indexes)))))



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

(defpolymorph-compiler-macro front (array) (&whole form container &environment env)
  (with-type-info (_ (array-type &optional elt-type dim) env) container
    (when-types ((array-type array)) form
      `(the (values ,elt-type &optional)
            (progn
              ,(cond ((eql dim 'cl:*)
                      (warn "An array should be of rank 1"))
                     ((< 1 (length dim))
                      (error "An array should be of rank 1")) ;;FIXME this doesn't trigger
                     (t t)) ;;instead sbcl check the aref against dimensions
              (aref ,container 0))))))                         ;; Not great, not terrible

(defpolymorph (setf front) ((new t) (container array)) t
  (setf (aref container 0) new))

;; TODO add compiler macros for setf front/back

(defpolymorph back ((container array)) t
              (assert (= 1 (array-rank container)))
              (aref container (1- (length container))))

(defpolymorph-compiler-macro back (array) (&whole form container &environment env)
  (with-type-info (_ (array-type &optional elt-type dim) env) container
    (when-types ((array-type array)) form
      `(the (values,elt-type &optional)
            (progn
              ,(cond ((eql dim 'cl:*)
                      (warn "An array should be of rank 1"))
                     ((< 1 (length dim))
                      (error "An array should be of rank 1"))
                     (t t))
              ,(once-only (container)
                 `(aref ,container (1- (length ,container)))))))))




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

(declaim (inline list-len))
(defun list-len (object)
  (declare (list object))
  (let ((count 0)
        (start object))
    (declare (type ind count))
    (loop :while object
          :do (incf count)
              (setf object (cdr object))
          :when (eq object start)
            :do (return))
    count))

(defpolymorph size ((object list)) (values ind &optional)
  (list-len object))

(defpolymorph capacity ((object list)) (values ind &optional)
  (list-len object))

(defpolymorph size ((object hash-table)) (values ind &optional)
  (hash-table-count object))

(defpolymorph capacity ((object hash-table)) (values ind &optional)
  (hash-table-size object))

