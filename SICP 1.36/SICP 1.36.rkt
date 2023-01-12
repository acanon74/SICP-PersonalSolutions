#lang racket/base

(define tolerance 0.00001)


(define (fixed-point f first-guess)

  (define (close-enough? v1 v2)
    
    (< (abs (- v1 v2)) tolerance)
  )
  
  (define (try guess)
    
    (newline)
    (display guess)
    (let ((next (f guess)))
      
      (if (close-enough? guess next)
          next
          (try next))
    )
  )
  (try first-guess)
)
(define (ef x)
  (/ (log 1000) (log x))
  )

(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)

(newline)
(newline)

(define (average x y)

  (/ 2 (+ x y)))


(define af (lambda (x) (average x (/ (log 1000) (log x)))))

;;(fixed-point af 10.0)

(define (x-to-the-x y) 
   (fixed-point (lambda (x) (average x (/ (log y) (log x)))) 
     10.0)) 