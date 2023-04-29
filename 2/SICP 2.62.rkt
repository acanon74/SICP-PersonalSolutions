#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)

  (cond ((or (null? set) (> (car set) x)) (cons x set))
        ((= (car set) x) set)
        ((< (car set) x) (cons (car set) (adjoin-set x (cdr set))))
        
        )
  )


(define (intersection-set set1 set2)

  (if (or (null? set1) (null? set2)) '()

      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= 1 x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2))))))
  )


(define (union-set set1 set2)

  (cond 
        ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1)
                                         (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1)
                                         (union-set (cdr set1) set2)))
        ((> (car set1) (car set2)) (cons (car set2)
                                         (union-set set1 (cdr set2))))
        )
  )

(define s (list 1 2 3))
(define r (list 4 5))

(define e '())

(adjoin-set 4 s)
(union-set s r)
(union-set (list 1 1 1) (list 4 4 4))
(union-set (list 1 2 7 10) (list 5 6 7 8 9))