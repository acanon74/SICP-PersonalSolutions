#lang sicp
(#%require sicp-pict)


(define p1 (cons 0 0))
(define p2 (cons 1 0))
(define p3 (cons 1 1))
(define p4 (cons 0 1))

(define v1 (make-vect 0 0))
(define v2 (make-vect 1 0))
(define v3 (make-vect 1 1))
(define v4 (make-vect 0 1))

(define s1 (make-segment v1 v2))
(define s2 (make-segment v2 v3))
(define s3 (make-segment v3 v4))
(define s4 (make-segment v4 v1))

(paint (segments->painter (list s1 s2 s3 s4)))

(define sx1 (make-segment v1 v3))
(define sx2 (make-segment v2 v4))

(paint (segments->painter (list sx1 sx2)))

(define mv1 (make-vect 0.5 0))
(define mv2 (make-vect 1 0.5))
(define mv3 (make-vect 0.5 1))
(define mv4 (make-vect 0 0.5))

(define ms1 (make-segment mv1 mv2))
(define ms2 (make-segment mv2 mv3))
(define ms3 (make-segment mv3 mv4))
(define ms4 (make-segment mv4 mv1))

(paint (segments->painter (list ms1 ms2 ms3 ms4)))