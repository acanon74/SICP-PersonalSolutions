#lang sicp

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ( (self-evaluating? exp)
          (analyze-self-evaluating exp) )
        ((amb? exp) (analyze-amb exp))
        ( (quoted? exp) (analyze-quoted exp) )
        ( (variable? exp) (analyze-variable exp) )
        ( (assignment? exp) (analyze-assignment exp) )
        ( (definition? exp) (analyze-definition exp) )( (if? exp) (analyze-if exp) )
        ( (let? exp) (analyze (analyze-let exp)))
        ( (lambda? exp) (analyze-lambda exp) )
        ( (begin? exp) (analyze-sequence (begin-actions exp) ) )
        ( (cond? exp) (analyze ( cond->if exp) ) )
        ( (application? exp) (analyze-application exp) )
        (else
         (error "Unknown expression type -- ANALYZE" exp) ) ) )

#|

(ambeval exp the-global-evironment (lambda (vakue fail) value)
(lambda () 'failed))

|#

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))


(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    
  (lambda (env succeed fail) (succeed qval fail))))


(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp) ) ) )
(lambda (env succeed fail)
  (vproc env
         (lambda (val fail2)
           (let ((old-value
                  (lookup-variable-value var env)))
             (set-variable-value! var val env)
             (succeed 'ok
                      (lambda ()
                        (set-variable-value! var
                                             old-value
                                             env)
                        (fail2)))))
         fail))))


(define (analyze-definition exp)
  (let ((var (definition-variable exp) )
        (vproc (analyze (definition-value exp) ) ) )
(lambda (env succeed fail)
  (vproc env
         (lambda (val fail2)
           (define-variable! var val env)
           (succeed 'ok fail2))
         fail))))


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))


(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))


(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))


(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))


(define (apply2 procedure arguments)

  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments) )
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure) ) ) )
        (else
         (error
          "Unknown procedure type -- APPLY2" procedure) ) ) )


(define (true? x)
(not (eq? x false) ) )
(define (false? x)
(eq? x false))


#|
(let ((x 3) (c 4)) ((* c x)))
|#


(define (let-list-pairs exp)
  (cadr exp))

(define (let-body exp)
 (cddr exp))

(define (let? exp)
  (tagged-list? exp 'let))



(define (let->combination list-variables body)


  (define variables (map (lambda (x) (car x)) list-variables))
  (define values (map (lambda (x) (cadr x)) list-variables))
  
  (cons (make-lambda variables body) values)
  )


(define (analyze-let exp)
  (let ((body (let-body exp))
        (variables (map (lambda (x) (car x)) (let-list-pairs exp)))
        (values (map (lambda (x) (cadr x)) (let-list-pairs exp))))
    (display variables)
    (newline)
    (display values)
    (newline)
    (display body)
    (newline)
    (display (cons (make-lambda variables body) values))
    (newline)
  (cons (make-lambda variables body) values))
  

)


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (ambeval (first-operand exps) env)
            (list-of-values (rest-operands exps) env) ) ) )

(define (eval-if exp env)
  (if (true? (ambeval (if-predicate exp) env) )
      (ambeval (if-consequent exp) env)
      (ambeval (if-alternative exp) env) ) )

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (ambeval (first-exp exps) env) )
        (else (ambeval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env) ) ) )

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                      (ambeval (assignment-value exp) env)
                      env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (ambeval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))




(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))




(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))


(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond) )

(define (cond-clauses exp) (cdr exp) )

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause) )

(define (cond-actions clause) (cdr clause) )

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses) )
            (rest (cdr clauses) ) )
        (if (cond-else-clause? first)
            (if (null? rest )
                (sequence->exp (cond-actions first) )
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first) )
                     (expand-clauses rest))))))



(define (make-procedure parameters body env)
(list 'procedure parameters body env))
(define (compound-procedure? p)
(tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())


(define (make-frame variables values)
(cons variables values))
(define (frame-variables frame) (car frame) )
(define (frame-values frame) (cdr frame) )
(define (add-binding-to-frame! var val frame)
(set-car! frame (cons var ( car frame) ) )
(set-cdr! frame (cons val (cdr frame)) ) )

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (<
           (length vars) (length vals) )
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env) ) )
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))

    (if (eq? env the-empty-environment)
        (error "Unbound variable " var)
    (let ((frame (first-frame env)))
      (scan (frame-variables frame)
            (frame-values frame)))))
(env-loop env))


(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
(cond ((null? vars)
       (env-loop (enclosing-environment env) ) )
      ((eq? var (car vars) )
(set-car! vals val))
(else (scan (cdr vars) (cdr vals) ) ) ) )
    
(if (eq? env the-empty-environment)
(error "Unbound variable -- SET ! " var)
(let ((frame (first-frame env)))
  (scan (frame-variables frame)
(frame-values frame) ) ) ) )
(env-loop env))


(define (define-variable! var val env)
(let ((frame (first-frame env) ) )
(define (scan vars vals)
(cond ((null? vars)
        (add-binding-to-frame! var val frame) )
((eq? var (car vars)) (set-car! vals val))
(else (scan (cdr vars) (cdr vals)))))
  
(scan (frame-variables frame) (frame-values frame))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (make-unbound! var env frame)

  (define exact-frame (car (filter (lambda (x) (eq? x frame)) env)))
  ; (if (null? exact-frame) (error "No frame in given environment" env))

  (define (scan vars vals start start2)
      
      (cond ((null? vars) (error "No variable bounded" var))
            ((eq? var (car vars)) (begin (set! vars (cons (reverse start) (cdr vars)))
                                        (set! vals (cons (reverse start2) (cdr vals)))
                                        'done))
      (else (scan (cdr vars) (cons (car vars) start) (cons (car vals) start2)))))
  
  (scan (frame-variables exact-frame) (frame-values  exact-frame) '() '()))


(define (primitive-procedure? proc)
(tagged-list? proc 'primitive) )
(define (primitive-implementation proc) (cadr proc) )

(define primitive-procedures
(list (list '+ +)
      (list '- -)
      (list '* *)
      (list '/ /)
      (list '< <)
      (list '> >)
      (list '<= <=)
      (list '>= >=)
      
      (list 'not not)
      (list 'list list)
      (list 'car car)
      (list 'cdr cdr)
      (list 'cons cons)
      (list 'null? null?)
      ))


(define (primitive-procedure-names)
  (map (lambda (x) (car x)) primitive-procedures))

(define (primitive-procedure-objects)
(map (lambda (proc) (list 'primitive (cadr proc)))
     primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))


(define input-prompt ";;; Amb-Eval input: " )
(define output-prompt ";;; Amb-Eval value: " )
(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))

  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(define (prompt-for-input string)
(newline) (newline) (display string) (newline) )
(define (announce-output string)
(newline) (display string) (newline) )


(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
(display object)))


;(eval (let ((x 2) (y 0)) (* x 6 y)) the-global-environment)

(driver-loop)
#|


(define (require p)
  (if (not p) (amb)))


(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between a b)

  (require (<= a b))
  (amb a (an-integer-between (+ a 1) b)))
|#