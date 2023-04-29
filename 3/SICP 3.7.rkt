#lang racket

(define (make-account-pw balance code)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (change-pw new-pw)
    (set! code new-pw))
  (define (auth pw op)
    (cond 
          ((eq? pw code) (dispatch op))
          ((and (not (eq? pw code)) (eq? op 'auth)) #f)
          ((and (not (eq? pw code)) (eq? op 'access)) "Bad Password! -Access")
          (else (dispatch 'badpw))))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'auth) #t)
          ((eq? m 'access) auth)
          ((eq? m 'new-pw) change-pw)
          ((eq? m 'badpw) (lambda (x) "Bad Password! -General"))
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  auth)



(define acc (make-account-pw 1000 '1234))


((acc '555 'deposit) 500)
((acc '1234 'deposit) 500)
((acc '1234 'withdraw) 1000)

(acc '555 'auth)
(acc '1234 'auth)
(acc '555 'access)
(acc '1234 'access)

(define pepe (acc '1234 'access))
((pepe '1234 'new-pw) 'qqq)

(pepe 'qqq 'auth)
(acc 'qqq 'auth)

(define (make-joint pw-acc pw new)
  

  (if (pw-acc pw 'auth) (begin ((pw-acc pw 'new-pw) new) (pw-acc new 'access)) "Couldn't Auth -Make-joint")
  
  )

(display "Actual Exercise:\n")

(define peter-acc (make-account-pw 200 'open-sesame))


(peter-acc 'open-sesame 'auth)
(peter-acc 'rose-bud 'auth)
((peter-acc 'open-sesame 'deposit) 100)

(display "After joint:\n")

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))


(paul-acc 'open-sesame 'auth)
(paul-acc 'rosebud 'auth)
((paul-acc 'open-sesame 'deposit) 100)
((paul-acc 'rosebud 'deposit) 200)

(peter-acc 'open-sesame 'auth)
(peter-acc 'rosebud 'auth)
((peter-acc 'open-sesame 'deposit) 100)
