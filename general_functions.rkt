#lang racket/base

(define (not= expr value)
  (not (eqv? expr value)))
(provide square)
(define (square x)
  (* x x))

