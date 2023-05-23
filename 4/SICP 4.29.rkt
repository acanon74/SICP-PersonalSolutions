#lang sicp


(define (eval exp env)
  
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((and? exp) (eval-and exp))
        ((or? exp) (eval-or exp))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env) )
        ((let? exp) (eval (let->combination (let-list-pairs exp) (let-body exp)) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply2 (actual-value (operator exp) env)
                 (operands exp)
                 env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply2 procedure arguments env)

  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure) ) ) )
        (else
         (error
          "Unknown procedure type -- APPLY2" procedure) ) ) )

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))


(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (true? x)
(not (eq? x false) ) )
(define (false? x)
(eq? x false))


#|
(let ((x 3) (c 4)) ((* c x)))
|#

;///////////////////

(define (let->combination list-variables body)


  (define variables (map (lambda (x) (car x)) list-variables))
  (define values (map (lambda (x) (cadr x)) list-variables))
  
  (cons (make-lambda variables body) values)
  )


(define (let-list-pairs exp)
  (cadr exp))

(define (let-body exp)
 (caddr exp))

(define (let? exp)
  (tagged-list? exp 'let))




;////////////////

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env) ) ) )

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env) )
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env) ) )

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env) )
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env) ) ) )

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                      (eval (assignment-value exp) env)
                      env)
  'ok1111)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok4324)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))


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


(define (and? exp) (tagged-list? exp 'and))


(define (eval-and exp)

  (cond  ((null? exp) #t)
         ((not (eval (first-operand exp))) #f)
         (else (eval-and (cdr exp))))
  )


(define (eval-and2 exp)

  (if (null? exp)
      #t

      (let ((first (car exp))
            (rest (cdr exp)))
      (make-if (cond-predicate (not first))
               #f
               (eval-and2 rest))
      )
  )
)


(define (or? exp) (tagged-list? exp 'or))

(define (eval-or exp)

  (cond  ((null? exp) #f)
         ((eval (first-operand exp)) #t)
         (else (eval-or (cdr exp))))

  )


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
  (list
      (list '+ +)
      (list '= =)
      (list '- -)
      (list '* *)
      (list '/ /)
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


(define input-prompt ";;; L-Eval input : " )
(define output-prompt ";;; L-Eval value : " )
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let  ((input (read)))
    (let ((output (actual-value input the-global-environment) ) )
      (announce-output output-prompt)
      (user-print output)))
(driver-loop))

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
(define (try a b)
(if (= a 0) 1 b))
(try 0 (/ 1 0))
|#





(define (fibo a b n i)

  (if (= i n)
      (+ a b)
      (fibo b (+ a b) n (+ i 1)))
  )

(fibo 1 1 4 0)


(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5)

#|

(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define (square x) (* x x))

Input:
(square (id 10))
(behind the curtains: count+1)
Output:
100
Input:
count
Output:
1
|#

#|
Above is my implementation, below is the wiki's

Here's a very simple, trivial example that illustrates the difference between memoizing and not memoizing thunks. First, assume that the force-it procedure has been defined with memoization, as in the longer of the two versions of force-it given in the book.

We also define a non-memoized version, which is simply the shorter of the two versions of force-it given in the book:

 (define (unmemoized-force-it obj) 
   (if (thunk? obj) 
       (actual-value (thunk-exp obj) (thunk-env obj)) 
       obj)) 
Here is our simple example, which can be run in MIT Scheme:

 (eval '(define (identity x) x) the-global-environment) 
  
 (eval '(define (identity-with-computation x) 
          (display " 1 hour elapsed ") 
          x) 
       the-global-environment) 
  
 (begin ;; with memoization 
   (eval '(define z (identity (identity-with-computation 0))) 
         the-global-environment) 
   (actual-value 'z the-global-environment) 
   (actual-value 'z the-global-environment)) 
 ;; displays "1 hour elapsed" *once* 
  
 ;; fluid-let is an MIT Scheme special form for dynamic (as opposed to lexical for ordinary let) binds 
  
 (fluid-let ((force-it unmemoized-force-it)) ;; without memoization 
   (eval '(define z (identity (identity-with-computation 0))) 
         the-global-environment) 
   (actual-value 'z the-global-environment) 
   (actual-value 'z the-global-environment)) 
 ;; displays "1 hour elapsed" *twice* 
What's happening here is that the argument given to identity, the expression '(identity-with-computation 0), is stored in a thunk via delay-it. With memoization, that thunk is effectively forced only once, but without it, that thunk is forced every time we call actual-value on z.

Note that memoization of thunks is similar, but *not exactly* the same thing as the memoization you may be used to in dynamic programming (the canonical example being the naive recursive algorithm for fibonacci, which is O(exp N) without dynamic programming (DP) memoization but O(N) *with* DP memoization.

In DP memoization, a function f(x) is computed *once* for every x; further calls to f(x) for the same x will simply return the stored value. For the thunk memoization discussed in this section of SICP, memoization is done for each *thunk object*, regardless of whether the the arguments are the same.

For example:

 (begin ;; with thunk memoization 
   (actual-value '(identity-with-computation 0) the-global-environment) 
   (actual-value '(identity-with-computation 0) the-global-environment)) 
 ;; displays "1 hour elapsed" *twice*, not once, even though both calls had the same argument 0. This is because both calls generated *separate* thunk objects. Thunk memoization does *not* help in this case! 
The takeaway here is that thunk memoization does *not* tabulate previously computed results by *argument*, the way dynamic programming memoization does. Note that DP memoization was actually implemented earlier in the book (chapter 3 if I remember correctly).

Hence, I would personally caution against using the O(exp N) naive recursive fibonacci algorithm for this exercise, since in that case, the multiple redundant calls of (fib n) to the same n are *not* sharing work, but are *still* redundantly doing work since they result in separate thunk objects. Even if thunk memoization *does* make a difference here, it is certainly not nearly as much as the jump from O(exp N) to O(N) that true dynamic programming memoization, which involves actually *tabulating computed results indexed by argument*.




ME:

Long story short, the guy is trying to say that
memoization here does not work based on the arguments to be
computed. But rather we do memoization per object.

The easiest way to see this is that memoization here
is done mutating the object state using set!. This could
be seen as if we are storing the now computed value
inside the object itself rather than builing a lookup
table that is used in DP.
Thus, even if we do eval to
two procedures with the same arguments, the object are
nonetheless NOT the same and therefore each will store
its own memoization copy.

|#
