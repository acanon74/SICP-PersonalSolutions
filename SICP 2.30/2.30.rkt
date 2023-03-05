#lang racket/base

(define (square x)
  (* x x))

(define l (list 1 (list 2 (list 3 4) 5)
                (list 6 7)))


(define (r tree f)
  (cond ((null? tree) '())
        ((not (pair? tree)) (f tree))
        (else (cons (r (car tree) f)
                    (r (cdr tree) f))))
  )



(newline)
(display l)
(r l square)
(newline)


(define x (list 1 2 3 4))

(define (rm tree f)

  (cond ((null? tree) '())
        ((not (pair? tree)) (f tree))
        (else (map (lambda (item) (rm item f)) tree))

        )

)

(newline)
(display x)
(rm x square)
(newline)

(newline)
(display l)
(rm l square)
(newline)