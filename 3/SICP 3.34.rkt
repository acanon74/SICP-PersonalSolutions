#lang racket
#|

Using the preestablished definition of multiplier is not
suitable for this problem.

In the case in which we have the value b and we wish
to calculate the value a, the process chain inside
multiplier will not be able to be completed since, from
the point of view of multiplier, a1 and a2
must be different entities and therefore checks for
has-value? in both accordingly. Consequently, if
we define a1 and a2 to be the same object, fact that
the interpreter doesn't know, when
calculating b multiplier will notice that a1 and a2
(both set to a) are both unknown and therefore
will default to undefined. 

|#