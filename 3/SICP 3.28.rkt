#lang racket

(require "circuits.rkt")


(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire value)
  ((wire 'set-signal!) value))
(define (add-action! wire action)
  ((wire 'add-action!) action))

(define inverter-delay (/ 1 1000))
(define and-gate-delay (/ 1 1000))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal -- LOGICAL-NOT" s))))

(define (logical-and a b)
  (cond  
         ((and (= a 1) (= b 1)) 1)
         (else 0)
  )
)

(define (inverter input ouput)
  
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! ouput new-value)))))
  (add-action! input invert-input)
  'ok)


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)


(define or-gate-delay (/ 1 1000))

(define (logical-or a b)
  (cond ((or (= a 1) (= b 1)) 1)
        (else 0)))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define a (make-wire))
(define b (make-wire))

(set-signal! a 0)
(set-signal! b 0)

(define c (make-wire))
(or-gate a b c)
(define r (or-gate a b c))

(a 'get-signal)
(display ",")
(b 'get-signal)
(c 'get-signal)
(newline)

(set-signal! a 0)
(set-signal! b 1)

(a 'get-signal)
(display ",")
(b 'get-signal)
(c 'get-signal)
(newline)
