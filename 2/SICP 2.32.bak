#lang racket/base

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest))))



  )


(define s (list 1 2 3))

(newline)
(display s)
(newline)
(define t (subsets s))
(display t)
