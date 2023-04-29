#lang racket/base

(define temp (list 1 2 3 4 5))


(define (last-pair items)

  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items)))

  )


(define (reverse items buffer)
  (if (null? items)
      buffer
      (reverse (cdr items) (append (cons (car items) '()) buffer))
      )
  

  )


temp
(reverse temp '())

(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse items buffer)


  (if (null? items)
      buffer
      
      (deep-reverse (cdr items) (append (cons (reverse (car items) '()) buffer)))

      )

)

(reverse x '())
(deep-reverse x '())