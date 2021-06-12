;;;; package.lisp

(defpackage #:polymorph.access
  (:use #:cl #:polymorphic-functions #:alexandria #:introspect-ctype)
  (:local-nicknames (:cm :sandalphon.compiler-macro))
  (:shadow #:emptyp)
  (:export #:at
           #:front
           #:back
           #:size
           #:capacity
           #:emptyp))
