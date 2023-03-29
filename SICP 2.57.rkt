#lang sicp

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2)) 
        (else (list '+ a1 a2)))) 

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) ;;(caddr s)

  (cond ( (> (length s) 3) (let ((temp (append (cons '+ nil) (cdddr s))))

                             (if (< (length temp) 3) (list '+ (caddr s) (cadr temp))
                                 (list '+ (caddr s) temp))
                             )
         )

        (else (caddr s))
        )  

  )

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))


(define (multiplicand p) ;;(caddr p)
  

  (cond ( (> (length p) 3) (let ((temp (append (cons '* nil) (cdddr p))))

                             (if (< (length temp) 3) (list '* (caddr p) (cadr temp))
                                 (list '* (caddr p) temp))
                             )
         )

        (else (caddr p))
        )


)
  

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp) (make-exponentiation (base exp) (make-sum (exponent exp) '-1))) (deriv (base exp) 'x)))
  (else (error "Unknown shit type -- DERIV" exp))))


(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (make-exponentiation b n)
  (cond ((=number? b 1) 1)
        ((=number? n 1) b)
        ((=number? n 0) 1)    
        ((and (number? base) (number? exponent)) (expt base exponent)) 
        
        (else (append (list '** b n)))
        
        )
  )
;;///////////////////////

 ;; tests
 (deriv '(* (* x y) (+ x 3)) 'x) 
 ;; (+ (* x y) (* y (+ x 3))) 
  
 (deriv '(* x y (+ x 3)) 'x) 
 ;; (+ (* x y) (* y (+ x 3)))

(deriv '(+ x 3) 'x)
;;1
(deriv '(* x y) 'x)
;;y
;;I think it's working, right???

(deriv '(+ x (* 3 (+ x (+ y 2)))) 'x)

(deriv '(+ (+ (* x y) 1) (+ (* (** x 2) 3) 4)) 'x) ;; (+ y (* (* 2 x) 3))

(deriv '(+ (* x y) 1 (* (** x 2) 3) 4)  'x)