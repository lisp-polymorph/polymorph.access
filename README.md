# Manual

- At
``` common-lisp
(define-polymorphic-function at (container &rest keys))
(define-polymorphic-function (setf at) (new container &rest keys))
```
Accessor to the containers, similar to `[]` in C-like languages. Works for arbitrary kyes as well as multiple indexes. Is SETFable.


- Front/Back

``` common-lisp
(define-polymorphic-function back (container))
(define-polymorphic-function front (container))
(define-polymorphic-function (setf front) (new container))
(define-polymorphic-function (setf back) (new container))
```
Front and back return first and last elements of the container respectively. Both are SETFable.



- Emptyp
``` common-lisp
(define-polymorphic-function emptyp (container))
```
Checks is a container is empty for a user point of view. Returns boolean.

- Size

``` common-lisp
(define-polymorphic-function size (container))
(define-polymorphic-function capacity (container))
```
Size returns the size of stored data inside the container. Capacity returns the upper limit of what can be currently stored in the container.
