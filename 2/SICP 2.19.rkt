#lang racket/base

(define (count-change amount coins)
  (cc amount coins))

(define (except-first-denomination items)
  (cdr items))

(define (first-denomination items)
  (car items))

(define (no-more? items)
  (if (null? items) #t #f) 
  )

(define (cc amount coins-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coins-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coins-values))
                 (cc (- amount
                        (first-denomination coins-values))
                     coins-values)))
        )
  )


(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(count-change 100 us-coins) ;;292
(count-change 100 uk-coins)


