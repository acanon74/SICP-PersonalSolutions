#lang sicp
#|

Since it is a stream, the second mention of THEASSERTIONS will
generate the next item in the stream. Esentially erasing it from
existance, this is not a desirable behavior so that's why we need
to store its old value using let.

I was wrong, here is the wiki:

  
 Because we use (cons-stream assertion THE-ASSERTION), so THE-ASSERTIONS will not be evaluated, (set! THE-ASSERTION (cons-stream assertion THE-ASSERTIONS)) will make THE-ASSERTION in the stream point to itself. so if we use THE-ASSERTIONS, it will lead to infinite loop. 

|#