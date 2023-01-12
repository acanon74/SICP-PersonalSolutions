#lang racket/base
(require "general_functions.rkt")


(define (square-list items)

  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items))))
  )

(define (b-square-list items)
  (map square items))

(square-list (list 1 2 3 4))
(b-square-list (list 1 2 3 4))