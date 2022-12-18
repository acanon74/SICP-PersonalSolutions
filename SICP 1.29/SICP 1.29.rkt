
#lang racket/base




;;(define h (/ (- b a) n))

(define (s a b n)


  (define (cube x) (* x x x))
  (define f cube)

  (define h (/ (- b a) n))

  (define (term x)
    (+ (f x) (* 2 (f (+ x h))) (* 4 (f (+ x (* 2 h))))
    )
  )

  (define (next x)
    (+ x (* 2 h))
  )


  (define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

  (* (/ h 3) (sum term a next b))

)


(define (t a b n)

  
  (define (cube x) (* x x x))
  (define h (/ (- b a) n))
  (define f cube)

  (define (term x)
    (+ (f x) (* 2 (f (+ x h))) (* 4 (f (+ x (* 2 h)))))
  )

  (define (next x)
    (+ x (* 2 h))

  )

  (define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))

  
  (* (/ h 3) (sum term a next b))
)

;;(t 0 1 100)
