#lang racket/base
(define temp (list 3 4 6 7 8))


(define (last-pair items)

  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items)))

  )


(last-pair temp)