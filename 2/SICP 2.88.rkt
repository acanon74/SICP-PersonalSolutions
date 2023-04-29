#lang racket
;(require "symbolic_algebra.rkt")

;dummy put
(define (put op type proc)
  (+ 1 1)
  )

#|
(define (install-scheme-number-package)
  (define (negative x) (make-scheme-number (* -1 x)))
  (define (substraction x y) (make-scheme-number (add x (negative y))))
  (put 'negative '(scheme-number) (lambda (x) (tag (negative x))))
  (put 'substraction '(scheme-number scheme-number) (lambda (x y) (tag (substraction x y))))
  )



(define (install-rational-package)

  (define (negative x) (make-rational (* -1 (numer x)) (denom x)))
  (define (add-rat x y) (+))
  (define (substraction x y) (make-rational (add-rat x (negative y))))
  (put 'negative '(rational) (lambda (x) (tag (negative x))))
  (put 'substraction '(rational rational) (lambda (x y) (tag (substraction x y))))
  )

(define (install-complex-package)
  (define (real-part x) (car x))
  (define (imag-part x) (cadr x))
  (define (negative x) ((make-from-real-imag (* -1 (real-part x)) (* -1 (imag-part x))))
  (define (substraction x y) ((make-from-real-imag (add x (negative y))))
  (put 'negative '(complex) (lambda (x) (tag (negative x))))
  (put 'substraction '(complex complex) (lambda (x y) (tag (substraction x))))
  )


 (define (negate-terms termlist) 
   (if (empty-termlist? termlist) 
         the-empty-termlist 
         (let ((t (first-term termlist))) 
           (adjoin-term (make-term (order t) (negate (coeff t))) 
                        (negate-terms (rest-terms termlist)))))) 
 (put 'negate 'polynomial 
          (lambda (poly) (make-polynomial (variable poly) 
                                          (negate-terms (term-list poly))))) 
 (put 'sub '(polynomial polynomial) 
       (lambda (x y) (tag (add-poly x (negate y))))) 

|#