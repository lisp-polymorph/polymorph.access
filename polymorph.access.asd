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
		 (:file "polymorph.access")))))
