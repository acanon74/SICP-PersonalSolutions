#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)


;procedure make-circle will make a circular list.
;The cdr of last cell of the list, instead of pointing
;to nil points to the first cell of the list.
;if we try to compute (last-pair z) will produce
;infinite recursion. 

;mcons, mcar, mcdr, set-mcar!, set-mcdr!, mlist


(define (append! x y)
  (set-cdr! (last-pair x) y) x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
(display (car z))
(display z)

(last-pair z)