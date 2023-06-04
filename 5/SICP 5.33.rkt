#lang racket
#|

(compile '

(define (factorial-alt n)
(if (= n 1)
1
(* n (factorial-alt (- n 1)))))

'val
'next
)

The generate machine code appears to be the same,
except for the following lines of code. This is due
to the how we treat symbols in compile.
This indeed makes factorial faster, because although in both
we use a stack save operation, old factorial will
have to do some assignments to val.

factorial-alt
false-branch4
...
   (save env)
...



factorial
false-branch4
...
   (assign val (op lookup-variable-value) (const n) (reg env))
   (assign argl (op list) (reg val))
   (save argl)
...


|#