#lang sicp

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (make-unbound! var env frame)
  ; My argument: we want to give the user maximum flexibility when managing resources
  ; Therefore, I would define make-unbounded! to unbound only the variable in a given
  ; Frame chose by the user. In this way, we guaranteed that there will be no
  ; Memory problems by forcing the user to define and use destructors in every
  ; Frame. Thus, this is at the expense of comfort for the user.

  ; I wouldn't check if the frame exist since we are already trusting the user to know what
  ; they are doing and to work with memory management.
  
  ; (define exact-frame (car (filter (lambda (x) (eq? x frame)) env)))
  ; (if (null? exact-frame) (error "No frame in given environment" env))

  (define (scan vars vals start start2)
      
      (cond ((null? vars) (error "No variable bounded" var))
            ((eq? var (car vars)) (begin (set! vars (cons (reverse start) (cdr vars)))
                                        (set! vals (cons (reverse start2) (cdr vals)))
                                        'done))
      (else (scan (cdr vars) (cons (car vars) start) (cons (car vals) start2)))))
  
  (scan (frame-variables  exact-frame) (frame-values  exact-frame) '() '()))