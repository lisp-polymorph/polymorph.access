;;; Unit tests for polymorph.access

(defpackage #:polymorph.access/test
  (:use #:cl
        #:fiveam
        #:polymorph.access)

  (:import-from #:alexandria
                #:alist-hash-table
                #:hash-table-alist
                #:set-equal)

  (:export #:polymorph.access
           #:test-polymorph.access))

(in-package #:polymorph.access/test)

;;; Test suite definition

(def-suite polymorph.access
    :description "Master test suite for polymorph.access")

(in-suite polymorph.access)

(defun test-polymorph.access ()
  (run! 'polymorph.access))


;;; Utilities

(defun test-at (seq &key (test #'equal))
  "Test that AT returns the same value, by comparison function TEST
for each element of SEQ as CL:ELT."

  (loop
        for i below (length seq)
        for item-elt = (elt seq i)
        for item-at = (at seq i)

        always
        (is (funcall test item-at item-elt)
            "(AT ~a ~a) returned: ~a~%Expected: ~a"
            seq i item-at item-elt)))

;;; Tests

;;;; Lists

(test list-at
  "Test AT on lists."

  (test-at '(a b c d)))

(test list-setf-at
  "Test (SETF AT) on lists."

  (let ((list (list 1 2 3 4)))
    (is (eq 'x (setf (at list 2) 'x)))
    (is (equal '(1 2 x 4) list))

    (is (eq 'a (setf (at list 0) 'a)))
    (is (equal '(a 2 x 4) list))))

(test list-front
  "Test FRONT on lists."

  (is (= 1 (front '(1 2 3 4 5 6))))
  (is (eq 'a (front (list 'a 'b 'c))))
  (is (equal "hello" (front (cons "hello" nil)))))

(test list-back
  "Test BACK on lists."

  (is (= 6 (back '(1 2 3 4 5 6))))
  (is (eq 'c (back (list 'a 'b 'c))))
  (is (equal "hello" (back (cons "hello" nil)))))

(test list-emptyp
  "Test EMPTYP on lists."

  (is (emptyp nil))
  (is (emptyp (cdr '(1))))
  (is (emptyp (list)))

  (is (not (emptyp '(1 2 3))))
  (is (not (emptyp (cdr (list 1 2))))))

(test list-size
  "Test SIZE on lists."

  (is-every =
    (4 (size '(a b c d)))
    (3 (size (list 1 2 3)))
    (2 (size (cons "a" (cons "b" nil))))
    (0 (size nil))
    (0 (size (cdr '(x))))))

(test list-capacity
  "Test CAPACITY on lists."

  (is-every =
   (0 (capacity nil))
   (5 (capacity '(1 2 3 4 5)))
   (3 (capacity (list 1 2 3)))))


;;;; Vectors (Single-dimensional arrays)

(test vector-at
  "Test AT on vectors (single-dimensional arrays)."

  (test-at #(a b c d))
  (test-at (vector 1 2 3 4 5 6 7 8 9 10))
  (test-at (make-array 5 :initial-contents '(1 2 3 4 5))))

(test vector-setf-at
  "Test (SETF AT) on vectors (single-dimensional arrays)."

  (let ((vec (vector 1 2 3 4)))
    (is (= 100 (setf (at vec 1) 100)))
    (is (equalp #(1 100 3 4) vec))))

(test vector-front
  "Test FRONT on vectors."

  (is (eq 'a (front #(a b c d))))
  (is (= 1 (front (coerce '(1 2 3) 'vector)))))

(test vector-back
  "Test BACK on vectors."

  (is (eq 'e (back #(a b c d e))))

  (let ((arr (make-array 10 :fill-pointer 0)))
    (vector-push 0 arr)
    (vector-push 1 arr)

    (is (= 1 (back arr)))))

(test vector-emptyp
  "Test EMPTYP on vectors."

  (is (emptyp #()))
  (is (emptyp (vector)))
  (is (emptyp (make-array 5 :fill-pointer 0)))

  (is (not (emptyp #(1 2 3))))
  (is (not (emptyp (vector 'a 'b 'c 'd 'e 'f))))
  (is (not (emptyp (make-array 1 :initial-element 0)))))

(test vector-size
  "Test SIZE on vectors."

  (is-every =
      (4 (size #(1 2 3 4)))
      (3 (size (make-array 7 :adjustable t :initial-element 0 :fill-pointer 3)))))

(test vector-capacity
  "Test CAPACITY on vectors."

  (is-every =
    (5 (capacity #(1 2 3 4 5)))
    (10 (capacity (make-array 10 :initial-element 0 :fill-pointer 7)))))


;;; Multi-dimensional Arrays

(test array-at
  "Test AT on multi-dimensional arrays."

  (is-every =
    (1 (at #2A((1 2) (3 4)) 0 0))
    (2 (at #2A((1 2) (3 4)) 0 1))
    (3 (at #2A((1 2) (3 4)) 1 0))
    (4 (at #2A((1 2) (3 4)) 1 1))))

(test array-setf-at
  "Test (SETF AT) on multi-dimensional arrays."

  (let ((array (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))))
    (is (= 100 (setf (at array 0 0) 100)))
    (is (= 200 (setf (at array 1 2) 200)))
    (is (equalp #2A((100 2 3) (4 5 200)) array))))

(test array-size
  "Test SIZE on multi-dimensional arrays."

  (is (= 9 (size (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (7 8 9))))))
  (is (= 8 (size #3A(((1 2) (3 4)) ((5 6) (7 8)))))))

(test array-capacity
  "Test CAPACITY on multi-dimensional arrays."

  (is (= 9 (capacity (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (7 8 9))))))
  (is (= 8 (capacity #3A(((1 2) (3 4)) ((5 6) (7 8)))))))


;;; Hash-tables

(test hash-table-at
      "Test AT on hash-tables."

      (let ((ht (alist-hash-table '((a . 1) (b . 2) (c . 3)))))
        (is (= 1 (at-safe ht 'a)))
        (is (= 2 (at-safe ht 'b)))
        (is (= 3 (at-safe ht 'c)))

        (is (eq nil (at-safe ht 'not-a-key)))
        (is (eq 'the-default (at-safe ht 'not-a-key 'the-default)))))

(test hash-table-setf-at
      "Test (SETF AT) on hash-tables."

      (let ((ht (alist-hash-table '((a . 1) (b . 2) (c . 3)))))
        (is (= 150 (setf* (at-safe ht 'a) 150)))
        (is (= 200 (setf* (at-safe ht 'new-key) 200)))

        (is (set-equal '((a . 150) (b . 2) (c . 3) (new-key . 200))
                       (hash-table-alist ht)
                       :test #'equal))))

(test hash-table-emptyp
  "Test EMPTYP on hash-tables."

  (is (emptyp (make-hash-table)))
  (is (not (emptyp (alist-hash-table '((a . 1)))))))
