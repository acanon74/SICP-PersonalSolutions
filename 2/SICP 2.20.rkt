#lang racket/base


(define (same-parity x . y)

  (define parity (modulo x 2))
  
  (define (rec y)

    (cond ((null? y) '())
          ((= (modulo (car y) 2) parity) (cons (car y) (rec (cdr y))))
          (else (rec (cdr y))))
    
  )

  (append (list x) (rec y))
  )


(define (iter-same-parity x . y)

  (define parity (modulo x 2))
  
  (define (iter y buffer)

    (cond ((null? y) buffer)
          ((= (modulo (car y) 2) parity) (iter (cdr y) (append (list (car y)) buffer)))
          (else (iter (cdr y) buffer))

    )
    
  )

  (iter y (list x))
  )


(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

(iter-same-parity 1 2 3 4 5 6 7)
(iter-same-parity 2 3 4 5 6 7)