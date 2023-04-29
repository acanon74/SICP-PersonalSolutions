#lang racket/base
(define (repeated f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeated f (- n 1))))
)


(define dx 0.00001)

(define (smooth f)
  
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))

)


(define (nth-fold-smooth f n)

  ((repeated smooth n) f)
)

