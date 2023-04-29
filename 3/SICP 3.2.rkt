#lang racket

(define (square x) (* x x))

(define (make-monitored f)

  (let ((counter 0))
  

    (define (increase value)
      (set! counter (+ counter 1))
      (f value))
    (define (dispatch m)
      (if (eq? m 'how-many-calls?) counter (increase m)) )
    dispatch
    )
    
  )

(define ms (make-monitored square))