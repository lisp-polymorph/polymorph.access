;;;; polymorph.access.asd

(asdf:defsystem #:polymorph.access
  :description "Access functions for polymorph.stl"
  :author "Commander Thrashdin"
  :license  "MIT"
  :version "0.5"
  :serial t
  :depends-on (#:polymorphic-functions
               #:compiler-macro
               #:introspect-ctype)
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
  :depends-on (#:polymorph.access #:polymorph.macros #:fiveam)
  :components ((:module
                "test"
                :serial t
                :components
                ((:file "util")
		 (:file "test"))))

  :perform (test-op (o s)
             (uiop:symbol-call '#:polymorph.access/test '#:test-polymorph.access)))
