#lang racket
(require "../arithmetic.rkt")

(define (random-init)
  (floor (round (* (random) (expt 2 32)))))

(define (rand-update x)
  (let ((a 1103515245)
        (c 12345)
        (m (expt 2 32)))
    (modulo (+ (* a x) c) m)))

(define rand (let ((x (random-init)))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)

  (= (gcd (rand) (rand)) 1))



(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    ;(display trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;(monte-carlo 10000 cesaro-test)

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


(define (inside-circle r)
  (let ((radius r))
    (lambda (x y)
      (if (<= (+ (expt (x) 2) (expt (y) 2)) (expt radius 2)) #t #f)))

  
  )

(define t (inside-circle 1))

(define (estimate-integral p x1 x2 y1 y2 n)

  (let ((xr (lambda () (random-in-range x1 x2)))
        (yr (lambda () (random-in-range y1 y2))))

    (let ((test (lambda () (p xr yr)))
          (area (* (- x2 x1) (- y2 y1))))

      (* (monte-carlo n test) area)
      
      )

    )
  )

(define result (estimate-integral (inside-circle 1) -1 1 -1 1 1000))
(display (exact->inexact result))


;Apparently, my accuracy is limited by some float to integer convertion
;somewhere in the predefine expression, I am not bothering with finding the
;appropiated expressions or languages or whatever.