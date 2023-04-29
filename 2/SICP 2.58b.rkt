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
        (else (list a1 '+ a2)))) 

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))


(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) ;;(caddr s)

  (cond ( (> (length s) 3) (let ((temp (cdddr s)))

                             (if (< (length temp) 3) (append (cons (caddr s) '()) temp)
                                 (append  (cons (caddr s) '()) temp))
                             ))
        (else (caddr s))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))


;;(deriv '(x + 3 * (x + y + 2)) 'x)
;; (+ (* x y) (* y (+ x 3)))


(define (multiplicand p) ;;(caddr p)
  

  (cond ( (> (length p) 3) (let ((temp (cdddr p)))

                             (if (< (length temp) 3) (append (cons (caddr p) '()) temp)
                                 (append  (cons (caddr p) '()) temp))
                             ))
        (else (caddr p))))
  

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
  (else (error "Unknown shit happened type -- DERIV" exp))))


(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (base exp) (car exp))
(define (exponent exp) (caddr exp))

(define (make-exponentiation b n)
  (cond ((=number? b 1) 1)
        ((=number? n 1) b)
        ((=number? n 0) 1)    
        ((and (number? base) (number? exponent)) (expt base exponent)) 
        
        (else (list b '** n))
        
        )
  )
;;///////////////////////

 ;; tests 
(deriv '(x + (3 * (x + (y + 2)))) 'x) 
;; (+ (* x y) (* y (+ x 3)))
(deriv '(x + 3 * (x + y + 2)) 'x)

(deriv '(x + 3) 'x);;1
(deriv '(x * y) 'x) ;;y


(display "TEST!!!!!!\n")
(deriv '(x * y * (x + 3)) 'x)
;Value 88: ((x * y) + (y * (x + 3)))

;; Will extraneous parens throw our deriv for a loop?
(deriv '((x * y) * (x + 3)) 'x)
;Value 89: ((x * y) + (y * (x + 3)))

(deriv '(x * (y * (x + 3))) 'x)
;Value 90: ((x * y) + (y * (x + 3)))

(deriv '(x * y + 1 + x ** 2 * 3 + 4) 'x) ;; (+ y (* (* 2 x) 3))
(deriv '(x * y + 1 + x * x * 3 + 4) 'x)
;;'(+ (+ (* x y) 1) (+ (* (** x 2) 3) 4))