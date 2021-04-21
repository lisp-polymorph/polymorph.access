;;;; polymorph.access.asd

(asdf:defsystem #:polymorph.access
  :description "Access functions for polymorph.stl"
  :author "Commander Thrashdin"
  :license  "CCA"
  :version "0.5"
  :serial t
  :depends-on (#:adhoc-polymorphic-functions #:compiler-macro)
  :components ((:module
		"src"
		:serial t
		:components
		((:file "package")
		 (:file "polymorph.access"))))

  :in-order-to ((asdf:test-op (asdf:test-op :polymorph.access/test))))

(asdf:defsystem #:polymorph.access/test
  :description "Unit tests for polymorph.access"
  :license "MIT"
  :serial t
  :depends-on (#:polymorph.access #:fiveam)
  :components ((:module
		"test"
		:serial t
		:components
		((:file "test"))))

  :perform (test-op (o s)
		    (uiop:symbol-call '#:polymorph.access/test '#:test-polymorph.access)))
