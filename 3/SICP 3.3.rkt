#lang racket

#|


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

|#

(define (make-account-pw balance code)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (auth pw op)
    (if (eq? pw code) (dispatch op) (dispatch 'badpw)))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'badpw) (lambda (x) "Bad Password!"))
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  auth)



(define acc (make-account-pw 1000 '1234))

((acc '543 'deposit) 500)
((acc '1234 'deposit) 500)
((acc '1234 'withdraw) 1000)