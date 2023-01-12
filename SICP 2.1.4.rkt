#lang racket/base

(define (make-interval a b)
  (if (or (= a 0) (= b 0))
      ((newline)
      (error "Cannot express an empty interval")
      (newline))
      (cons a b)
      ))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))



(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (map-interval interval f)
  (make-interval (f (lower-bound interval)) (f (upper-bound interval))))

(define (print-interval interval)
  (newline)
  (display (lower-bound interval))
  (display " - ")
  (display (upper-bound interval)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;Ex 2.12
(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (average x y)
  (/ (+ x y) 2))

(define (percent interval)

  (define midpoint (average (upper-bound interval) (lower-bound interval)))
  (define width (abs (- midpoint (lower-bound interval))))

  (/ (* 100 width) midpoint)

    
  )

(define (reciprocal x)
  (/ 1.0 x))

(define (square x)
  (* x x))


(define t1 (make-interval 6.12 7.48))
(define t2 (make-interval 4.46 4.93))

(print-interval t1)
(print-interval t2)

;;(print-interval (map-interval (add-interval (map-interval t1 reciprocal) (map-interval t2 reciprocal)) reciprocal))
;; 2.57 - 2.97

;;Ex 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(print-interval (add-interval t1 t2))
(print-interval (sub-interval t1 t2))
(percent t1)

(define temp (mul-interval t1 t2))

(print-interval temp)

(* (- 1 0.1) (- 1 0.05) 6.8 4.7)