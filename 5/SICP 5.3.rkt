#lang sicp
(controller
 (assign guess (const 1.0))
 sqrt-iter
 (test (op good-enough?) (reg guess) (reg x))
 (branch (label done))
 (assign t (op improve) (reg guess) (reg x))
 (assign guess (reg t))
 (goto (label sqrt-iter))

 done
)