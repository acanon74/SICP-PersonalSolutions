#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (reverse x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '())
  )



(define v (mlist 'a 'b 'c 'd))
(display v)
(define w (reverse v))
(newline)
(display w)

;The mistery function reverses the order of the list.