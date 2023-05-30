#lang sicp
(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s) ) )


(define (simple-flatten stream)
  (stream-map stream-car
               ( stream-filter (lambda (x) (not (null? x))) stream) ) )


#|

No the order will not change. 

|#