;;; Unit test utilities

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

;;; Utilities

(defparameter *test-levels*
  '((normal (speed 1) (safety 1) (space 1) (debug 1))
    (fast (speed 3) (safety 0) (debug 0)))

  "List of optimize levels on which the tests should be run.

Each item is a list where the first element is a symbol naming the set
of optimization levels and the remaining elements are the optimize
levels which are placed in a `(DECLARE (OPTIMIZE ...)) expression.")

(defmacro test-optimize (name &body body)
  "Define a test which is run on multiple optimization levels.

This form is equivalent to FIVEAM:TEST with the difference being that
multiple tests are generated, one for each set of optimization levels
in *TEST-LEVELS*.

For each item (LEVEL-NAME . LEVELS) in *TEST-LEVELS*, a separate test
is generated with the name of the test prepended with the level name,
given by LEVEL-NAME, and the test forms wrapped in
a `(LOCALLY (DECLARE (OPTIMIZE . ,@LEVELS)) ...)."

  (multiple-value-bind (forms declarations docstring)
      (alexandria:parse-body body :documentation t)

    (destructuring-bind (name &rest args)
	(alexandria:ensure-list name)

      `(progn
	 ,@(loop for (level . optimize) in *test-levels*
	      collect
		`(test (,(alexandria:symbolicate level '- name) ,@args)
		   ,@(alexandria:ensure-list docstring)

		   (locally
		       (declare (optimize ,@optimize))
		     ,@declarations

		     ,@forms)))))))

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
