#lang sicp
(#%require sicp-pict)

(define e (flip-horiz einstein))
(define m mark-of-zorro)




(define (split op op2)

  (lambda (painter) (op painter (op2 painter painter)))

  )



(define right-split2 (split beside below))
(define up-split2 (split below beside))
(paint (right-split2 e))
(paint (up-split2 e))