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


(define (a-pythagorean-triple-between low)
  (let ((i (an-integer-starting-from low)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-starting-from j)))
        (require (and (<= i j)
                      (= (+ (* i i) (* j j ) ) (* k k))))
        (list i j k)))))
;(a-pythagorean-triple-between 0)

(define (a-pythagorean-triple-between2 low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        ((lambda (x) (if (not x) (amb) (a-pythagorean-triple-between2 low (+ high 1)))) (+ (* i i) (* j j)) (* k k))
        (list i j k)))))


#|
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))
|#

#|

a. If we replace the procedure, we get rid of the upper
bound in every sequence. Thus, eventually, by typing
try-again we WILL generate an iteration in which
i is bigger than k, regardless of the initial value of k.
This violates the given condition that i <= j.

My solution is above, it is not the same as the wiki so I do not expect it to work.
I will try it out later once I write the support code.
WIKI:

  (define (a-pythagorean-triple-greater-than low) 
    (let ((i (an-integer-starting-from low))) 
      (let ((j (an-integer-between low i))) 
        (let ((k (an-integer-between low j))) 
          (require (= (+ (* k k) (* j j)) (* i i))) 
          (list k j i))))) 

|#