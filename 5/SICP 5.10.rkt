#lang sicp
#|
From the wiki, this ex is too straigthforward.

|#

;; as an example - instead of (assign x (op +) (reg a) (reg b)) I want 
 ;; the syntax: (assign x (apply + (reg a) (reg b))) 
  
 (define (operation-exp? exp) 
   (tagged-list? exp 'apply)) 
 (define (operation-exp-op exp) 
   (cadr exp)) 
 (define (operation-exp-operands exp) 
   (cddr exp))