#lang racket/base

(define (filter predicate sequence) 
   (cond ((null? sequence) '()) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence))))) 

(define (accumulate op initial sequence)

  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

 (define (enumerate-interval low high) 
   (if (> low high) 
       '()
       (cons low (enumerate-interval (+ low 1) high)))) 

(define (prime? x) 
   (define (test divisor) 
     (cond ((> (* divisor divisor) x) #t) 
           ((= 0 (remainder x divisor)) #f) 
           (else (test (+ divisor 1))))) 
   (test 2)) 

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))



(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n))

  )


(define (new n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(define (sum-trios trio)
  (list (car trio) (car (cdr trio)) (car (cdr (cdr trio))) (accumulate + 0 trio)  ))

(define (tri n q)
  (map sum-trios (filter (lambda (u) (if (= (accumulate + 0 u) q) #t #f)) (flatmap (lambda (i) (map (lambda (k) (append i (list k))) (enumerate-interval 1 (- (car (cdr i)) 1)))) (unique-pairs n) ))

  ))

(tri 5 12)
(display "W")

(display (list 1 2 3 4 5))

(define (displayer n q)

  (if (> q n) ((display "\nEND") (error "Finished execution"))
      ((newline)
       (display (tri n q))
       (display q)
       (displayer n (+ 1 q))))



  )

(displayer 12 1)