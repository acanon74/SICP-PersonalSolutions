#lang sicp


(define (require p)
  (if (not p) (amb)))


(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between a b)

  (require (<= a b))
  (amb a (an-integer-between (+ a 1) b)))