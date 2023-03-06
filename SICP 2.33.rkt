#lang racket/base

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))



(define (map-alt p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))



(define (append-alt seq1 seq2)
  (accumulate cons seq2 seq1) )


(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))



(define (square x)
  (* x x))

(map-alt square (list 1 2 3 4))
(define q (list 1 2 3))
(define w (list 4 5 6))


(append-alt q w)

(length q)