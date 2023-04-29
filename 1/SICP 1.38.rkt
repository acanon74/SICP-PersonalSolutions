#lang racket/base
(define n (lambda (x) 1.0))
(define d 1.0)

;;Basically first exercises I did entirely by myself :D


(define (f n d k)

  (if (= k 0) 0

      (if (or (even? d) (= d 1))
      (/ (n k)
         (+ d (f n (+ d 1) (- k 1))))
      
      (/ (n k)
         (+ 1 (/ (n k)
                 (+ 1 (f n (+ d 1) (- k 1))))))

  ))
    
)

(+ (f n d 1000) 2)