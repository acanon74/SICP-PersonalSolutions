#lang sicp
#|

We should feed the matcher a history of the frames containing
past inferences, to do this, we must past variables and their
respective bindings.

To actually check for loops, everytime we are asked to interpret
something new, we would first check whether that specific pattern
has been asked for already (instantiating variables for values first).
If it is duplicated, we mark this as a loop and terminate.
If it is not, we can continue to the evaluation part and so on.


WIKI:

information should at least include :
1. rule name

2. variable bindings, both bounded (to values or other variables)
and unbouned

if the rule is already in processing and
 the 1 and 2 information above is the same with
 the current attempt to apply after unification, then stop.
|#