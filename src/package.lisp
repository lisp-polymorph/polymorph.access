;;;; package.lisp

(defpackage #:polymorph.access
  (:use #:cl #:polymorphic-functions #:alexandria #:introspect-ctype)
  (:shadow #:emptyp)
  (:export #:at
           #:front
           #:back
           #:size
           #:capacity
           #:emptyp))
