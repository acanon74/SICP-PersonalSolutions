#lang racket/base

(define t (list (list 3 4) (list 5 5)))
(define b (list 3 4))

(define (accumulate op initial sequence)

  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (n op init seqs)

  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (n op init (map cdr seqs))))

  )

(define (sum lst)
  (cond ((null? lst) 0)      ; base case: empty list has sum of 0
        (else (+ (car lst)   ; add the first element to the sum
                 (sum (cdr lst))))))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) 
          (dot-product v w)) m)

  )

(matrix-*-vector t b)

(define tr (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define (transpose mat)
  (n (lambda (x y) (cons x y)) '() mat)

  )

(transpose tr)

(define a (list (list 1 2) (list 3 4)))
(define c (list (list 5 7) (list 6 8)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n))) 
     (map (lambda (v) (matrix-*-vector cols v)) m))

  )

(matrix-*-matrix a c)

;;I HATE THIS BULLSHIT DUDE!