#lang racket/base
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (line point1 point2)
  (cons point1 point2))

(define (make-segment start end)
  (line start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment) ) (x-point (end-segment segment))) 2) (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2))
)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")

)



(define q (make-point 1 2))
(define p (make-point 5 4))

(define function (make-segment q p))

(midpoint-segment function)
