#lang racket/base

(define (square x)
  (* x x))

(define l (list 1 (list 2 (list 3 4) 5)
                (list 6 7)))

(define x (list 1 2 3 4))

(define (tree-map f tree)

  (cond ((null? tree) '())
        ((not (pair? tree)) (f tree))
        (else (map (lambda (item) (tree-map f item)) tree))

        )

)

(define (square-tree tree) (tree-map square tree))

(newline)
(display x)
(square-tree x)
(newline)

(newline)
(display l)
(square-tree l)
(newline)