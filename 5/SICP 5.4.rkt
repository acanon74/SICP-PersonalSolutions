#lang sicp

(controller
 (assign continue (label done))
 
 loop
 (test (op =) (reg n) (const 0))
 (branch (label base))

 (save continue)

 (assign continue (label after))
 (assign n (op -) (reg n) (const 1))
 (goto (label loop))

 after
 (restore continue)
 (assign val (op *) (reg val) (reg b))
 (goto (reg continue))
 
 base
 (assign val (const 1))
 (goto (reg continue))

 done


 )
