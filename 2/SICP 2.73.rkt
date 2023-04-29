#lang racket

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

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;a: We generalized the operation deriv by using a look-up table to index the different
; Differentiation laws based on the tag that defines exp.

;a: We expect numbers and single variables not to have a tag, in this representation a tag
;is an arithmetic operation, so since no tag, no way to make it work with the look-up table. Also, it is inefficient as shit.

(define (deriv-sum exp var)

  ;(display exp)
  (make-sum (deriv (car exp) var)
            (deriv (cadr exp) var)
            )
  )

(define (deriv-product exp var)


  (make-sum (make-product (car exp) (deriv (cadr exp) var))
            (make-product (deriv (car exp) var) (cadr exp))

            )
  )

;(put op type item)
;(get op type)
(put 'o 'test 'hi)

(get 'o 'test)

;b:
(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-product)
(deriv '(+ (* x x) 1) 'x)
;c:

(define (make-exponentiation b n)
  (cond ((=number? b 1) 1)
        ((=number? n 1) b)
        ((=number? n 0) 1)    
        ((and (number? base) (number? exponent)) (expt base exponent)) 
        
        (else (append (list '** b n)))
        
        )
  )

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

(define (deriv-exponent exp var)

  (make-product (make-product (cadr exp)
                              (make-exponentiation (car exp) (make-sum (cadr exp) '-1)))
                (deriv (car exp) 'x))
  )

(put 'deriv '** deriv-exponent)

(deriv '(+ (** x 3) (* x 3)) 'x)

;d: Only the order of the procedures when inserting an element in the look-up table


(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp)
                                           var))))
(put '** 'deriv deriv-exponent)
(put '* 'deriv deriv-exponent)
(put '+ 'deriv deriv-exponent)
(deriv '(+ (** x 3) (* x 3)) 'x)