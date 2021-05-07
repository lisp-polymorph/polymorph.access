;;;; package.lisp

(defpackage #:polymorph.access
  (:use #:cl #:adhoc-polymorphic-functions #:alexandria #:polymorph.utility)
  (:local-nicknames (:cm :sandalphon.compiler-macro))
  (:shadow #:emptyp)
  (:export #:at
           #:front
           #:back
           #:size
           #:capacity
           #:emptyp))
