#lang sicp
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops )
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0) )
    (define (push x)
      (set! s (cons x s ) )
      (set! number-pushes (+ 1 number-pushes ) )
      (set! current-depth (+ 1 current-depth) )
      (set! max-depth (max current-depth max-depth) ) )
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP" )
          (let ((top (car s ) ) )
            (set! s (cdr s) )
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes
                     '= number-pushes
                     'maximum-depth '= max-depth) ) )
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop) )
            ((eq? message 'initialize) (initialize) )
            ((eq? message 'print-statistics)
             (print-statistics) )
            (else
             (error "Unknown request -- STACK" message) ) ) )
    dispatch) )

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents!
   (get-register machine register-name) value) 'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (print-ninst n)
  (newline)
  (display (list 'n-instructions '= (get-contents n))))


(define (reset-ninst n)
  (set-contents! n 0))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (ninst (make-register 'ninst)))

    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))
                 (list 'print-ninst
                       (lambda () (print-ninst ninst)))
                 (list 'reset-ninst
                       (lambda () (reset-ninst ninst)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag) (list 'ninst ninst))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine )
                    insts)))


(define (exist? label labels)
  (not (assoc label labels))
  )

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (if (exist? next-inst labels)
                                  (receive insts
                                           (cons (make-label-entry next-inst
                                                                   insts)
                                                 labels))
                                  (error "Duplicated label in controller -- EXTRACT-LABELS" next-inst))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))


(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc) )
        (flag (get-register machine 'flag) )
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts )
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))


(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst ) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc) )
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc) )
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc) )
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc) )
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc) )
        ((eq? (car inst ) 'perform)
         (make-perform inst machine labels ops pc) )
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc machine)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))



(define (advance-pc pc machine)
  (set-register-contents! machine 'ninst (+ 1 (get-register-contents machine 'ninst)))
  (set-contents! pc (cdr (get-contents pc)))
  )

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc machine)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    ( if (label-exp? dest)
         (let ((insts
                (lookup-label labels (label-exp-label dest))))
           (lambda ()
             (if (get-contents flag)
                 (set-contents! pc insts)
                 (advance-pc pc machine))))
         (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))


(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc machine))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc machine))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))



(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc machine)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))


(define (perform-action inst) (cdr inst))


(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (register-exp? exp) (tagged-list? exp 'reg) )
(define (register-exp-reg exp) (cadr exp) )
(define (constant-exp? exp) (tagged-list? exp 'const ) )
(define (constant-exp-value exp) (cadr exp) )
(define (label-exp? exp) (tagged-list? exp 'label) )
(define (label-exp-label exp) (cadr exp) )


(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations ) )
        (aprocs
         (map (lambda (e)   
                (if (label-exp? e)
                    (error "Procedures cannot be applied to labels -- MAKE-OPERATION-EXP" e)
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))


(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))
;549


#|

(define expt-machine (make-machine 'registers 'operations-pairs
                                              'instr-list)
(set-register-contents! expt-machine 'b 2)
(start expt-machine)
(get-register-contents expt-machine 'val)
|#

(define i '(
            (assign ninst (const 0)) ;1
            (perform (op print-ninst)) ;2 
            (perform (op initialize-stack)) ;3
            (assign continue (label fact-done))
            
            fact-loop
            (test (op =) (reg n) (const 1) )
            (branch (label base-case) )
            (save continue)
            (save n)
            (assign n (op -) (reg n) (const 1 ) )
            (assign continue (label after-fact) )
            (goto (label fact-loop) )
            after-fact
            (restore n)
            (restore continue)
            (assign val (op *) (reg n) (reg val) )
            (goto (reg continue) )
            base-case
            (assign val (const 1) )
            (goto (reg continue) )
            fact-done
            (perform (op print-stack-statistics))
            (perform (op print-ninst))
            (perform (op reset-ninst))
            (perform (op print-ninst))
            ))

(define r '(continue n val))

(define o (list
           (list '= =)
           (list '- -)
           (list '* *)))

(define m (make-machine r o i))

(define (driver-loop)

  (let ((number (read)))
    (if (eq? number 'quit)
        'closing-machine
        (begin
          (set-register-contents! m 'n number)
          (set-register-contents! m 'val 0)
        (start m)
        (get-register-contents m 'val)
        
        (driver-loop))))
    
  )

(driver-loop)

#|
From the book we know that it is linear. From executing we see that
both totalpushes and maximumdepth grow the same, so their equation will
be the same.

We can just model this as a linear equation an + b = result.
then we just take any to results where n > 1 and solve the system of
equation

We obtain a = 2 and b = -2. So the equation of grow is 2(n-1) = result
|#