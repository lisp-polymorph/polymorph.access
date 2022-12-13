# Manual

- At
``` common-lisp
(define-polymorphic-function at (container &rest keys))
(define-polymorphic-function (setf at) (new container &rest keys))
```
At is the accessor to the containers, similar to `[]` in C-like languages. Works for arbitrary keys as well as multiple indexes. Is SETFable. Has an `:error` option, which is on by default for arrays/lists and off for hash-tables. With `:error t` at will error for non existent keys, with `:error nil` it will not error and just returns two values, second of which indicates if the key was valid.


- Front/Back

``` common-lisp
(define-polymorphic-function back (container))
(define-polymorphic-function front (container))
(define-polymorphic-function (setf front) (new container))
(define-polymorphic-function (setf back) (new container))
```
Front and back return the first and last elements of the container respectively. Both are SETFable.



- Emptyp
``` common-lisp
(define-polymorphic-function emptyp (container))
```
Emptyp checks if a container is empty from a user's point of view. Returns a boolean.

- Size

``` common-lisp
(define-polymorphic-function size (container))
(define-polymorphic-function capacity (container))
```
Size returns the current size of stored data inside the container. Capacity returns the upper limit of what can be currently stored in the container.
