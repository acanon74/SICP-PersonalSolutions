#lang sicp

;;This code works, tested using machine.rkt

(define expt-machine (make-machine '(n b val continue)
                                   (list
                                    (list '= =)
                                    (list '* *)
                                    (list '- -)
                                    )
                                   '(
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
                                   ))


(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 3)
(set-register-contents! expt-machine 'continue 'done)

(start expt-machine)

(get-register-contents expt-machine 'val)