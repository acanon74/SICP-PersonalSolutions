#lang sicp



(define (equal? a b)

  
  
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else #f)


  )

)



;;(equal? '(this is a list) '(this is a list)) ;; #t
(equal? '(this is a list) '(this (is a) list)) ;; #f

 (equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6)) 
 ;Value: #t 
  
 (equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5 7) 6)) 
 ;Value: #f 