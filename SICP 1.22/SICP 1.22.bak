#lang racket/base

(define (runtime) (current-inexact-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (prime? (+ n 2)))
)

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
)

(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
  ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))



(define (test-range a b)

  (if (even? a)
      (test-range (+ a 1) b)
      (cond ((> a b)
             (newline) (display "DONE"))
            (else (timed-prime-test a)
                  (test-range (+ a 2) b))
      )
  )
)
