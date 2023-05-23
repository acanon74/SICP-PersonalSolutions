#lang sicp
#|

count
1

w
10

(delayed (id 10)) -> basically a promise to generate (id 10)
should it be needed somewhere.


count
2


The strange behavior of the variable count happens not because of the first expression in the body of id (e.g. "identity"), but rather the *second*. To see what I mean, let's first examine the example from the exercise. In order to provide more clarity, I will call eval and actual-value directly as opposed to using the driver-loop REPL provided in the book.

 (let ((env the-global-environment)) 
   (eval '(define count 0) env) 
   (eval '(define w (id (id 10))) env) 
   (assert (= 1 (eval 'count env))) 
   (assert (= 10 (actual-value 'w env))) 
   (assert (= 2 (eval 'count env)))) 
Here, the 10 and 2 are trivial and require no explanation. The 1 is most interesting: it is because when we eval the expression (id (id 10)) in env when we are first defining w, the procedure list-of-delayed-args has x in the body of id bound to the result of (delay-it '(id 10) env), a thunk. Since (eval-definition exp env) calls eval, not actual-value, on (definition-value exp), it is the thunk that is bound to w, not the number 10. That thunk is not forced until we call actual-value on w, and hence before doing that, count has been incremented only once.

Note we have used eval for the assert lines involving count, since count is always bound to a number, but I have used actual-value for the assert involving w, since at that moment w is bound to a thunk and must be forced. If we had tried to call eval on w immediately after defining it, eval would return the thunk object, and attempting to print it may result in an infinite recursion loop since the thunk object contains env, which is a list that may contain cycles.

|#