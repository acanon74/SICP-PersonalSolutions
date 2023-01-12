#lang racket/base

(define (for-each f items)
  (cond ((null? items) #t)
        (else (begin (f (car items)) (for-each f (cdr items)))))
  
  )


(for-each (lambda (x) (newline) (display x)) (list 57 321 88))