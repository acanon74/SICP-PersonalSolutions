#lang racket/base


(define (make-rat n d)
  (cons n d)
)

(define (numer x)
  (car x))
(define (denom x)
  (cdr x))

(define (better-rat n d)

  (if (and (or (< n 0) (< d 0)) (not (and (< n 0) (< d 0))))
      (begin (set! n (* -1 (abs n)))
       (set! d (abs d)))

      (begin (set! n (abs n))
       (set! d (abs d)))
  )
  (make-rat n d)

)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y)))
)

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y)))
)

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y)))
)

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y)))
)

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)