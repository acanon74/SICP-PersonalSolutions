#lang racket

(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

#|

WITHOUT SERIALIZING:

Let the instructions be named as:


a set! x [first one]
b (* x x)
c set! x [second one]
d (* x x x)


Then we can have the following executions:

Where the order of the letters indicates the order of the procedure
and a "X" by the number indicates that the procedures executes a
set! instruction, updating the value in memory of the variable X.

badc - 10e2 - x10e2 - 10e6 - x10e6  
bdac - 10e2 - 10e3 - x10e2 - x10e3
bdca - 10e2 - 10e3 - x10e3 - x10e2

dcba - 10e3 - x10e3 - 10e2 - x10e2
dbca - 10e3 - 10e2 - x10e3 - x10e2
dbac - 10e3 - 10e2 - x10e2 - x10e3

Possible outcomes are 10e6, 10e3, 10e2.


WITH SERIALIZING:

Let the instructions be named as:

a (* x x)
b (* x x x)

We can have the following executions:

ab - x10e2 - x10e6
ba - x10e3 - x10e6

With seriliazing the only possible solution is 10e6.


Apparently this is wrong since the procedure request access to
the variable X for symbol, so the value of x might change
mid-evaluation to something like * 10 100 10000 which I did not
considered, since the environment model doesnt work that way.
|#

