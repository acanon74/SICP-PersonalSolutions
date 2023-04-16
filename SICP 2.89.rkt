#lang racket


(require "arithmetic.rkt")


(define t (list 1 2 0 3 -2 -5))


(define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))


(define (the-empty-termlist) '())
(define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))


(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))


(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(first-term t)
(first-term (cdr t))