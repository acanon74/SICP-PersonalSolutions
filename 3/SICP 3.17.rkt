#lang racket


(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

#|

Another one from the wiki, I made sure to understand
the problem and the solution before looking up the
solution.

I copy from the wiki because they are very good with
ASCII art and I am not.


|#




(define (count-pairs x d) 

  (define (is-in? key items)
    (cond ((null? items) #f)
          ((eq? (car items) key) #t)
          (else (is-in? key (cdr items)))
          )
  )

  (cond ((not (pair? x)) 0)

        ((is-in? x d) (+ (count-pairs (car x) d)
                  (count-pairs (cdr x) d)))
        
         (else (+ (count-pairs (car x) (append x d))
                  (count-pairs (cdr x) (append x d))
                  1))
         
  )


)


(define (counter2 x)

  (let ((helper '()))
    
    (define (is-in? key items)
    (cond ((null? items) #f)
          ((eq? (car items) key) #t)
          (else (is-in? key (cdr items)))
     ))
    

      (define (count-pairs x d)

        (cond ((not (pair? x)) 0)
              ((is-in? x d) (+ (count-pairs (car x) d)
                               (count-pairs (cdr x) d)))

        (else (+ (count-pairs (car x) (append x d))
                 (count-pairs (cdr x) (append x d))
                 1))
         
  )


)
      (count-pairs x helper)

      
  )


)



(define str1 '(foo bar baz)) 
(count-pairs str1 '()) ; 3 



 (define x '(foo)) 
 (define y (cons x x)) 
 (define str2 (list y)) 
 (count-pairs str2 '()) ; 4 


 (define r '(foo)) 
 (define t (cons r r)) 
 (define str3 (list (cons t t))) 
 (count-pairs str3 '()) ; 7 


(define str4 '(foo bar baz))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

 (set-cdr! (list->mlist (last-pair str4)) str4) 
 (count-pairs str4 '()) ; maximum recursion depth exceeded

(display "NEW")
(newline)

(counter2 str1)
(counter2 str2)
(counter2 str3)
(counter2 str4)