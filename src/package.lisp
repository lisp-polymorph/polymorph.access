;;;; package.lisp

(defpackage #:polymorph.access
  (:use #:cl #:polymorphic-functions #:alexandria #:introspect-ctype)
  (:shadow #:emptyp)
  (:export #:at
           #:at-safe
           #:front
           #:front-safe
           #:back
           #:back-safe
           #:size
           #:capacity
           #:emptyp))
