;;;; polymorph.access.lisp

(in-package #:polymorph.access)

;;; At
(define-polymorphic-function at (container &rest keys) :overwrite t
  :documentation "Return the element of the container specified by the keys.")
(define-polymorphic-function (setf at) (new container &rest keys) :overwrite t
  :documentation "Setf for at.")
(define-polymorphic-function at-safe (container &rest keys) :overwrite t
  :documentation "if the keys are valid for container acess return the
 associated value and t as multiple values, otherwise return nil and nil")
(define-polymorphic-function (setf at-safe) (new container &rest keys) :overwrite t
  :documentation "Setf for at-safe. Only works on containers that allow
 adding element by arbitary key")


(defpolymorph (at :inline t) ((array array) &rest indexes) t
  (apply #'aref array indexes))

(defpolymorph-compiler-macro at (array &rest) (array &rest indexes &environment env)
  (with-array-info (elt-type _) array env
    (declare (ignorable _))
    (if (constantp (length indexes) env)
        `(the (values ,elt-type &optional) (aref ,array ,@indexes))
        `(the (values ,elt-type &optional) (apply #'aref ,array ,indexes)))))


(defpolymorph ((setf at) :inline t) ((new t) (array array) &rest indexes) t
  (setf (apply #'aref array indexes) new))



(defpolymorph-compiler-macro (setf at) (t array &rest) (new array &rest indexes &environment env)
  (with-array-info (elt-type _) array env
    (declare (ignorable _))
    (print elt-type)
    (let ((new-type (%form-type new env)))
      (cond ((not (subtypep new-type elt-type env))
             (error 'type-error :expected-type elt-type :datum new))
            ((constantp (length indexes) env)
             (print
              `(the (values ,elt-type &optional) (setf (aref ,array ,@indexes)
                                                      (the (values ,elt-type &optional) ,new)))))
            (t
             `(the (values ,elt-type &optional) (setf (apply #'aref ,array ,indexes)
                                                      (the (values ,elt-type &optional) ,new))))))))


(defpolymorph (at-safe :inline t) ((array array) &rest indexes) t
  (if (apply #'array-in-bounds-p array indexes)
      (values (apply #'aref array indexes) t)
      (values nil nil)))

(defpolymorph-compiler-macro at-safe (array &rest) (array &rest indexes &environment env)
  (with-array-info (elt-type _) array env
    (declare (ignorable _))
    (if (constantp (length indexes) env)
        (let ((names (loop :for _ :in (cons array indexes) :collect (gensym))))
          `(let ,(loop :for obj :in (cons array indexes)
                       :for name :in names
                       :collect `(,name ,obj))
             (if (array-in-bounds-p ,@names)
                 (the (values ,elt-type boolean) (values (aref ,@names) t))
                 (values nil nil))))
       `(if (apply #'array-in-bounds-p ,array ,indexes)
           `(the (values ,elt-type &optional) (apply #'aref ,array ,indexes))
            (values nil nil)))))






(defpolymorph at ((list list) &rest indexes) t
  (apply #'elt (the cons list) indexes))



(defpolymorph-compiler-macro at (list &rest) (list &rest indexes &environment env)
  (if (constantp (length indexes) env)
      `(elt ,@indexes ,list)
      `(apply #'elt ,indexes ,list)))



(defpolymorph (setf at) ((new t) (list list) &rest indexes) t
  (assert (not (cdr indexes)) nil 'simple-error :format-control "List access takes 1 index")
  (let ((list (nthcdr (car indexes) list)))
    (if list
        (setf (car list) new)
        (error 'simple-error :format-control "Index not in list bounds"))))



(defpolymorph-compiler-macro (setf at) (t list &rest) (&whole form
                                                              new list &rest indexes
                                                              &environment env)
  (if (constantp (length indexes) env)
      (let ((ls (gensym)))
        (assert (= 1 (length indexes)) nil 'simple-error :format-control "List access takes 1 index")
        `(let ((,ls (nthcdr ,(car indexes) ,list)))
           (if ,ls
               (setf (car ,ls) ,new)
               (error 'simple-error :format-control "Index not in list bounds"))))
      form))

#||
(defpolymorph at-safe ((list list) &rest indexes) t
  (apply #'elt (the cons list) indexes))


(defpolymorph-compiler-macro at (list &rest) (list &rest indexes &environment env)
  (if (constantp (length indexes) env)
      `(nth ,@indexes ,list)
      `(apply #'nth ,indexes ,list)))
||#



(defpolymorph at-safe ((ht hash-table) &rest indexes) (values t boolean &optional)
  (apply #'gethash (first indexes) ht (cdr indexes)))


(defpolymorph-compiler-macro at-safe (hash-table &rest) (&whole form ht &rest indexes &environment env)
                             (if (constantp (length indexes) env)
                                 (once-only (indexes)
                                            `(gethash ,(first indexes) ,ht ,(second indexes)))
                                 form))

(defpolymorph (setf at-safe) ((new t) (ht hash-table) &rest indexes) t
  (setf (apply #'gethash (first indexes) ht (cdr indexes)) new))

(defpolymorph-compiler-macro (setf at-safe) (t hash-table &rest) (&whole form
                                                                         new ht &rest indexes
                                                                         &environment env)
  (if (constantp (length indexes) env)
      (once-only (indexes)
        `(setf (gethash ,(first indexes) ,ht ,(second indexes)) ,new))
      form))



(define-setf-expander at (container &rest indexes &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion container env)
    (declare (ignorable setter))
    (values dummies
            vals
            newval
            `(funcall #'(setf at) ,@newval
                      (the ,(%form-type container env) ,getter) ,@indexes)
            `(at (the ,(%form-type container env) ,getter) ,@indexes))))

(define-setf-expander at-safe (container &rest indexes &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion container env)
    (declare (ignorable setter))
    (values dummies
            vals
            newval
            `(funcall #'(setf at-safe) ,@newval
                      (the ,(%form-type container env) ,getter) ,@indexes)
            `(at-safe (the ,(%form-type container env) ,getter) ,@indexes))))




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
  (with-array-info (elt-type dim) container env
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

;; TODO add compiler macros for setf front/back

(defpolymorph back ((container array)) t
              (assert (= 1 (array-rank container)))
              (aref container (1- (length container))))

(defpolymorph-compiler-macro back (array) (container &environment env)
  (with-array-info (elt-type dim) container env
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

(defmacro setf* (place val &environment env)
  (if (symbolp place)
      `(setq ,place ,val)
       (multiple-value-bind (temps exprs stores store-expr access-expr)
           (get-setf-expansion place env)
         ;(print store-expr) (print temps) (print exprs) (print stores) (print access-expr)
         (declare (ignorable access-expr))
         (if temps
             `(let* ((,@temps ,@exprs)
                     (,@stores ,val))
                (declare (type ,(%form-type (car exprs) env) ,@temps)
                         (type ,(%form-type val env) ,@stores))
                ,store-expr)
             `(let* ((,@stores ,val))
                (declare (type ,(%form-type val env) ,@stores))
                ,store-expr)))))
