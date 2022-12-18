#lang racket/base


(define (product term a next b)

  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))

  )

)

(define (term_a x)
  (* x x)
)

(define (next_a x)
  (+ x 2)
)


(define (div n)


  (* 2 (/ (product term_a 2 next_a n)
       (product term_a 3 next_a n)))

)

;;(real->double-flonum (div 10000))