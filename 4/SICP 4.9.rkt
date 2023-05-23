#lang sicp
(define t (list 1 2 3 4 5 6))

(define (eval-while predicate action otherwise args)

  (define (init args)
    (display args)
(if (predicate args) (action args) (otherwise init args))

    )

  (init args)
  )

(define pre (lambda (args) (if (null? args) #f (= (car args) 4))))
(define act (lambda (args) 'found))

(define other 
  (lambda (iter args) (if (null? args) 'not-found (iter (cdr args))))
  )


(eval-while pre act other t)

;(define (while? exp) (tagged-list? exp 'while))


(define (while-predicate exp) (cadr exp))
(define (while-action exp) (caddr exp))
(define (while-otherwise exp) (cadddr exp))


(define (while-parameters exp) (list (cadr exp) (caddr exp) (caddr exp)))
(define (while-data exp) (cadddr exp))

(define (lambda-body exp) (cddr exp))


;eval: ((while? exp) (eval-while exp))