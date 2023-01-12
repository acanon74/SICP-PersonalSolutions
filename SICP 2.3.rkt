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

(define (square x)
  (* x x))

(define (get-diagonal x y)
  (sqrt (+ (square (- (car y) (car x))) (square (- (cdr y) (cdr x)))))
)

(define (get-distance x y axis)
  (- (axis y) (axis x)))

;;Implemented as 4 lines
(define (make-rectangle a b c d)
  (cons (cons a b) (cons c d)))

(define (a rectangle)
  (car (car rectangle)))

(define (b rectangle)
  (cdr (car rectangle)))

(define (c rectangle)
  (car (cdr rectangle)))

(define (d rectangle)
  (cdr (cdr rectangle)))

(define (get-height rectangle)
  (get-distance (start-segment (a rectangle)) (end-segment (a rectangle)) y-point))

(define (get-length rectangle)
  (get-distance (start-segment (b rectangle)) (end-segment (b rectangle)) x-point))

;;(define (get-area rectangle)
  ;;(* (get-height rectangle) (get-length rectangle)))

;;(define (get-perimeter rectangle)
  ;;(+ (* 2 (get-height rectangle)) (* 2 (get-length rectangle))))


(define point1 (make-point 0 0))
(define point2 (make-point 0 2))
(define point3 (make-point 4 2))
(define point4 (make-point 4 0))

(define a-side (make-segment point1 point2))
(define b-side (make-segment point2 point3))
(define c-side (make-segment point3 point4))
(define d-side (make-segment point4 point1))



(define q (make-rectangle a-side b-side c-side d-side))

(get-height q)
(get-length q)

;;(get-area q)
;;(get-perimeter q)

;;Implemented as a pair of precomputed height and length, plus the points.
(define (better-rectangle a b c d)
  (define height (get-distance a b y-point))
  (define length (get-distance b c x-point))
  (define dimensions (cons height length))
  
  (cons dimensions (cons (cons a b) (cons c d)))
  )

(define m (better-rectangle point1 point2 point3 point4))

(define (better-height rectangle)
  (car (car rectangle)))

(define (better-length rectangle)
  (cdr (car rectangle)))

;;If I were to implement a new rectangle completely, I could just change the previous definition of get-height. Or If I were required to support old rectangle functions,
;;I would change area, perimeter definitions to be flexible in how they get height and length values, and doing so, leaving multiplication and addition as the only
;;task they perform, I will implement this change below.

;;In the other approach, I would have to only change get-height and get-length to the new better-height and better-length, and not change get-area and get-perimeter at all.

(define (get-perimeter rectangle f-height f-length)
  (* (f-height rectangle) (f-length rectangle)))

(define (get-area rectangle f-height f-length)
  (+ (* 2 (f-height rectangle)) (* 2 (f-length rectangle))))

(display "Old implementation")
(newline)

(get-area q get-height get-length)
(get-perimeter q get-height get-length)

(display "New implementation")
(newline)

(get-area m better-height better-length)
(get-perimeter m better-height better-length)