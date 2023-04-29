#lang racket/base

(define (inc x)
  (+ x 1))

(define (compose f g)
  (lambda (x)
    (f (g x))))


(define (square x)
  (* x x))

(define (repeated f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeated f (- n 1))))
)


((repeated square 2) 5)
(newline)
((repeated inc 3) 0)