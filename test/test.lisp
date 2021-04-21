;;; Unit tests for polymorph.access

(defpackage #:polymorph.access/test
  (:use #:cl #:alexandria #:fiveam)
  (:export #:polymorph.access
           #:test-polymorph.access))

(in-package #:polymorph.access/test)

;;; Test suite definition

(def-suite polymorph.access
    :description "Master test suite for polymorph.access")

(in-suite polymorph.access)

(defun test-polymorph.access ()
  (run! 'polymorph.access))
