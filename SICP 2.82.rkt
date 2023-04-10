#lang racket

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))


(define *coercion-table* (make-hash))

(define (put-coercion coercion type proc)
  (hash-set! *coercion-table* (list coercion type) proc))

(define (get-coercion coercion type)
  (hash-ref *coercion-table* (list coercion type) '()))


 (define (type-tag datum) 
   (cond ((number? datum) datum) 
         ((pair? datum) (car datum)) 
         (else (error "Wrong datum -- TYPE-TAG" datum)))) 
  
 (define (contents datum) 
   (cond ((number? datum) datum) 
         ((pair? datum) (cadr datum)) 
         (else (error "Wrong datum -- CONTENGS" datum)))) 
  
 (define (attach-tag tag content) 
   (cond ((number? content) content) 
         ((pair? content) (cons tag content))))


(define (apply-generic op . args) 
   (define (no-method type-tags) 
     (error "No method for these types" 
       (list op type-tags))) 
  
   (let ((type-tags (map type-tag args))) 
     (let ((proc (get op type-tags))) 
       (if proc 
           (apply proc (map contents args)) 
           (if (= (length args) 2) 
               (let ((type1 (car type-tags)) 
                     (type2 (cadr type-tags)) 
                     (a1 (car args)) 
                     (a2 (cadr args))) 
                 (if (equal? type1 type2) 
                   (no-method type-tags) 
                   (let ((t1->t2 (get-coercion type1 type2)) 
                         (t2->t1 (get-coercion type2 type1)) 
                         (a1 (car args)) 
                         (a2 (cadr args))) 
                     (cond (t1->t2 
                            (apply-generic op (t1->t2 a1) a2)) 
                           (t2->t1 
                            (apply-generic op a1 (t2->t1 a2))) 
                           (else (no-method type-tags)))))) 
               (no-method type-tags))))))



