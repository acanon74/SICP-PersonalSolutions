#lang racket/base
(define (accumulate op initial sequence)

  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(accumulate / 1 (list 1 2 3))

(fold-left / 1 (list 1 2 3))

(accumulate list '() (list 1 2 3))

(fold-left list '() (list 1 2 3))


;;Based on multiplication. Indeed, division not you idiot... 