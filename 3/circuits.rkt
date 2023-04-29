#lang racket

(define (after-delay time proc)
  (sleep time)
  (proc))

(define (make-wire)

  (let ((state 0)
        (actions (list)))

    (define (set-signal! value)

      (if (= state value)
          (set! state value)
          (begin (set! state value) (run-actions actions)))
      
      )

    (define (add-action! proc)

      (set! actions (cons proc actions))
      (proc)
      )
    (define (run-actions l)

      (if (not (null? l))
          (begin ((car l)) (run-actions (cdr l)))
          'executed)
      )
    
    (define (dispatch m)

      (cond ((eq? m 'get-signal) state)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'add-action!) add-action!)
            ((eq? m 'actions-list) actions)
            (else (error "Invalid procedure -- WIRE" m)))
      )
    dispatch
    )
  )

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

#|
(define a (make-wire))
(define w 3)

(a 'get-signal)
((a 'set-signal!) 1)
(a 'get-signal)
(a 'actions-list)
((a 'add-action!) (lambda () (set! w 44)))
(a 'actions-list)

(display "wwwwwwwwwww\n")

(define b (make-wire))
;(define i (inverter a b))


(a 'get-signal)
(display ",")
(b 'get-signal)
(newline)

(set-signal! a 0)

(a 'get-signal)
(display ",")
(b 'get-signal)
(newline)


(set-signal! a 1)

(a 'get-signal)
(display ",")
(b 'get-signal)
(newline)

(display "QQQQQQQQQQQQQQ/n")

(set-signal! a 0)
(set-signal! b 0)

(define c (make-wire))

(define o (and-gate a b c))

(a 'get-signal)
(display ",")
(b 'get-signal)
(c 'get-signal)
(newline)

(set-signal! a 1)
(set-signal! b 1)

(a 'get-signal)
(display ",")
(b 'get-signal)
(c 'get-signal)
(newline)

(display "RRRRRRRRRRRRRRRRRRR/n/n")

(define a (make-wire))
(define b (make-wire))
|#

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

#|

(set-signal! a 0)
(set-signal! b 0)

(define v (make-wire))

(define p (or-gate a b v))

(a 'get-signal)
(display ",")
(b 'get-signal)
(v 'get-signal)
(newline)

(set-signal! a 0)
(set-signal! b 1)

(a 'get-signal)
(display ",")
(b 'get-signal)
(v 'get-signal)
(newline)

|#

(provide (all-defined-out))