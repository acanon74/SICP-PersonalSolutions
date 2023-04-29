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

(define (displayer n q)

  (if (> q n) ((display "\nEND") (error "Finished execution"))
      ((newline)
       (display (tri n q))
       (display q)
       (displayer n (+ 1 q))))

  )

;;(tri 20 30)

(define (measure-time f x p)
  (let ([start (current-milliseconds)])
    (f x p)
    (- (current-milliseconds) start)))

;;(flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n))
;;(enumerate-interval 1 (- (car (cdr i)) 1))
;;general (- n 1) (- k 1) 1
;;(enumerate-interval 1 (- (car (cdr i)) 1)))) (unique-pairs n)
(define (general n k)

  (cond ((= k 0) ('()))
        ((= k 1) (enumerate-interval 1 n))
        (else (flatmap (lambda (i) (map (lambda (j) (cons i j)) (general (- i 1) (- k 1)))) (enumerate-interval 1 n))
))
  

)
(unique-pairs 3)
(general 3 2)
  
(define (newnew n q)

  (flatmap (lambda (i) (if (= (+ (car i) (car (cdr i)) (car (cdr (cdr i)))) q) i null)) (general n 3))



  )



  
;;(tri 20 30)
(display "NEWNEW\n")
;;(newnew 20 30)
(displayln (measure-time tri 100 30))  ; prints the time in milliseconds to compute
(displayln (measure-time newnew 100 30))