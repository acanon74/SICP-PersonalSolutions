#lang racket
;(require "symbolic_algebra.rkt")

;dummy put
(define (put op type proc)
  (+ 1 1)
  )

#||#
(define (install-scheme-number-package)
  (define (=zero? x) (= x 0))
  (put '=zero? '(scheme-number) =zero?)
  )



(define (install-rational-package)
  (define (numer x) (car x))
  (define (=zero? x) (= (numer x) 0))
  (put '=zero? '(rational) =zero?)
  )

(define (install-complex-package)
  (define (real-part x) (car x))
  (define (imag-part x) (cadr x))
  (define (=zero? x) (and (= (real-part x) 0) (= (imag-part x) 0)))
  (put '=zero? '(complex) =zero?)
  )