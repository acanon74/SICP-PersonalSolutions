#lang racket


(require "arithmetic.rkt")


(define t (list 1 2 0 3 -2 -5))


(define (adjoin-term term term-list past)
  (display term-list)
  (newline)

  

    (cond

      ((=zero? (coeff term)) term-list)
      ((and (null? term-list) (null? past)) (adjoin-term term (cons 0 term-list) past))
      ((and (< (length term-list) (order term)) (null? past)) (adjoin-term term (cons 0 term-list) past))
      
      ((= (length term-list) (order term)) (let ((current (car term-list)))
                                                   (if (= current 0)
                                                       (append (append past (list (coeff term))) (cdr term-list))
                                                       (append (append past (list (+ current (coeff term)))) (cdr term-list))))))
      

      ((> (length term-list) (order term)) (adjoin-term term (cdr term-list) (append past (list (car term-list)))))
      

      

)







(define (the-empty-termlist) '())
(define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))


(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))


(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(first-term t)
(first-term (cdr t))

(define r '())
(define r1 (adjoin-term (make-term 2654 -5) r '()))

;(display (adjoin-term (make-term 5 1) '() '()))


;Shit doesnt work but I have gotten the idea, also it seems that no one in the wiki has actually figure it out. They are treating it like a normal append, or
;using a translation layer of just converting this type to the other (order coeff) type, that is not what the book wants since the additivity thing,
;also, it wont allow you to work properly with ad-join. Moreover, they dont realize that, specially in mul-term, they might appear order of variables
;that are not ordered as an ordered list, given this, their assumpting that we do not have to check for populated orders or for sum of coefficients
;will miss, take for example multiplyinh two polynomials, when you do it, you do it term by term regardless if there exists same-order terms, that is x*(x^2 +1) = x^3 = x
;we do not need there to be another x^1 in the second poly. This will not work in their code because it will collide with a 0 coeff.