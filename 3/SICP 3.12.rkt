#lang racket
#|

(define x (list 'a 'b))
(define y (list 'c 'd))

(define z (append x y))

z ; (a b c d)

(cdr x)

Since this was constructed using append, which simply
creates a new list assigning copies of the values
to a newly constructed pairs, we can be sure that the
values of x will remained intact.
The answer is (b)


(define w (append! x y))

w ; (a b c d)

Since this was constructed using append!, which cdr-set!s
the cdr of x to the second list y, we can be sure that
(cdr x) no longer points to b and nil,
rather, it points to y and in consequence it owns the nil
in y.

The answer is (b c d)

;

|#