#lang racket
(define (make-accumulator input)

  (let ((total input))

    (lambda (value) (begin
                  (set! total (+ total value))
                  total))
    )
  
  )

(define a (make-accumulator 5))