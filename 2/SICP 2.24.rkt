#lang racket/base
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))



(count-leaves (list 1 (list 2 (list 3 4))))


(define l (list 1 3 (list 5 7) 9))

(cdr (car (cdr (cdr l))))

