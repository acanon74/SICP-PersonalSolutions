#lang sicp
(define (run-forever) (run-forever) )
(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))


#|

It violates the expected behavior of halts? no matter
the state of halts?

Suppose such a procedure halts? exists, then
suppose (halts? try try) actually halts, then
the if conditional set the machine in a run-forever
state, directly contradicting the halts?
expected output.

The proof for the case in which (halts? try try) doesnt
halts is similarly obtained.

|#