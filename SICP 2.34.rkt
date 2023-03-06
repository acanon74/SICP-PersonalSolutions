#lang racket/base

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))




(horner-eval 2 (list 1 3 0 5 0 1))
;;79
(horner-eval 2 (list 7 8 9 2 4 1))
;;171
(horner-eval 0 (list 7 8 9 2 4 1))
;;7
(horner-eval 1 (list 7 8 9 2 4 1))
;;31