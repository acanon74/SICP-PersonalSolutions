#lang racket
#|

Basically two processes check the mutex at the same time.

If the second one checks before the first one has executed
its set-car! expression to true, then both will see the mutex as
false and concurrently set! the mutex to true, case in which
BOTH processes get access to the mutex at the same time.


|#