#lang sicp
#|

(define (equ? n m)


  ;Now, eq? only checks same-type elements, therefore, we should implement eq?
  ;per type in their respective install file.
  ;And install this into the arithmetic package.
  
  (if (= (tag n) (tag m)) (eq? n m) #f)
  )

Just call equ? with parameters m = data, n = 0.
Again, it is upto the specific data types to implement what does it mean
to be zero, for a rational number it's its numerator = 0, for a complex
its magnitude = 0. and so on.

Retrieve from the wiki, example of the specific implementations I propose

 (define (=zero? x) (apply-generic '=zero? x)) 
  
 ;; add into scheme-number-package 
 (put '=zero? 'scheme-number (lambda (x) (= x 0))) 
  
 ;; add into rational-number-package 
 (put '=zero? 'rational-number  
          (lambda (x) (= (numer x) 0))) 
  
 ;; add into complex-number-package 
 (put '=zero? 'complex-number 
          (lambda (x) (= (real-part x) (imag-part x) 0))) 



|# 