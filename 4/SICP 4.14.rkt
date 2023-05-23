#lang sicp


#|

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

I believe the problem to be in the former function,
when Eva uses her implementation of map, she
must define selectors to extract the appropiate
parameters of map from args. She would write
something like this

(define (map-proc exp) (cadr exp))
(define (map-arg exp) (caddr exp))

and would comfortably execute (map procedure argument)

On the other hand,

Louis would use the former function, which
as we can see would pass args as a single argument
rather than filtering the procedure and argument,
in other words, it would try to execute
(map args) Which doesn't work with the underlying
definition of map.

|#