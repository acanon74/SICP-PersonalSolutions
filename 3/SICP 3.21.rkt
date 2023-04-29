#lang racket
(require "queue.rkt")

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)


(define (print-queue q) 
   (define (iter x) 
     (if (null? x) 
         (newline) 
         (begin (display (mcar x)) 
                (iter (mcdr x))))) 
   (iter (front-ptr q))) 
    
  
  

(define w (make-queue))
(insert-queue! w 'a)
(insert-queue! w 'b)
(insert-queue! w 'c)
(print-queue w)

