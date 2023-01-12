#lang racket/base

(define (not= expr value)
  (not (eqv? expr value)))

(define (make-pair a b)
  (* (expt 2 a) (expt 3 b))
)

;;(not (eqv? (modulo n 2) 0)))

(define (a-car n counter)
  (if (or (= n 1) (not= (modulo n 2) 0)) counter
      (a-car (/ n 2) (+ counter 1)))
)

(define (a-cdr n counter)

  (if (or (= n 1) (not= (modulo n 3) 0)) counter
      (a-cdr (/ n 3) (+ counter 1)))

)

(define p (make-pair 2 3))
(define q (make-pair 11 17))

(a-car p 0)
(a-cdr p 0)

(a-car q 0)
(a-cdr q 0)
