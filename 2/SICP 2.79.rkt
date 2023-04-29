#lang sicp



(define (equ? n m)


  ;Now, eq? only checks same-type elements, therefore, we should implement eq?
  ;per type in their respective install file.
  ;And install this into the arithmetic package.
  
  (if (= (tag n) (tag m)) (eq? n m) #f)
  )

#|

Retrieve from the wiki, I propose specific implementation of the procedure eq? for
each type. This guarantees that multi implementation types like complex will take
care of calling the appropiate selectors for their specific implementations.

 (define (install-scheme-number-package) 
   ;; ... 
   (put 'eq? '(scheme-number scheme-number) =) 
   'done) 
  
 (define (install-rational-package) 
   ;; ... 
   (define (eq? x y) 
     (= (* (numer x) (denom y)) (* (numer y) (denom x)))) 
   ;; ... 
   (put 'eq? '(rational rational) equ?) 
   'done) 
  
 (define (install-complex-package) 
   ;; ... 
   (define (eq? x y) 
     (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))) 
   ;; ... 
   (put 'eq? '(complex complex) equ?) 
   'done)

|#
