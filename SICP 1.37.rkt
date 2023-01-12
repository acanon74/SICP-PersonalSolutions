#lang racket/base
;;(lambda (i) 1.0)

(define d 1.0)

(define n 1.0)


(define (f n d k)

  (if (= k 0)
      1
      (/ n (+ d (f n d (- k 1))))
   )
    
)


(define c (/ 2 (+ 1 (sqrt 5))))

(display c)
(newline)

(f n d 10)

(define (cont-frac n d k) 
   (define (loop result term) 
     (if (= term 0) 
         result 
         (loop (/ (n term) 
                  (+ (d term) result)) 
               (- term 1)))) 
  
   (loop 0 k)) 

(define (i-f n d k)

  (define (loop result i)
    (if (= i 0)
        result
        (loop (/ n (+ d result)) (- i 1)))
  )

  (loop 0 k)

)


(newline)

;;k must be > 10 for the i-f to be correct to 4 dec places
(define m (lambda (x) 1.0))
(define b (lambda (x) 1.0))

(cont-frac m b 10)

(newline)

(i-f n d 10)