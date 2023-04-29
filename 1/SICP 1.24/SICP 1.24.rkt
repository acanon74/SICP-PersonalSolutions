#lang racket/base

(define (runtime) (current-inexact-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))
      (fast-prime? (+ n 2) 10))
)

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
)



(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m)
         )
  )

)


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a)
  )
  (try-it (+ 1 (random (- n 1))))
)


(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)
  )
)


(define (square n)
  (* n n))



(define (smallest-divisor n)
  (find-divisor n 3))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
  ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (+ test-divisor 2)))))

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



