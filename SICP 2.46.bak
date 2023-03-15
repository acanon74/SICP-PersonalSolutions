#lang sicp
(#%require sicp-pict)


(define (make-vect x y)

  (cons x (cons y nil))
  )

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (car (cdr vect)))

(define t1 (make-vect 1 2))
(define t2 (make-vect 3 4))

(define (neg-vect vect)
  (scale-vect vect -1))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (let ((neg-v2 (neg-vect v2)))
    (add-vect v1 neg-v2)))

(define (scale-vect vect scalar)
  (make-vect (* (xcor-vect vect) scalar) (* (ycor-vect vect) scalar)))

(neg-vect t2)
(add-vect t1 t2)
(sub-vect t1 t2)
(scale-vect t1 4)
