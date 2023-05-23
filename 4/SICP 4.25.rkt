#lang sicp
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))


(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

(factorial 5)


In applicative-order Scheme, when call (factorial 5), the call will not end. because, when call unless, even if (= n 1) is true, (factorial (- n 1)) will be called. so n will be 5, 4, 3, 2, 1, 0, -1 .... . In normal-order Scheme, this will work, Because normal-order Scheme uses lazy evaluation, when (= n 1) is true, (factorial n) will not be called. 