#lang racket

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))


(define (type-tag datum) 
   (cond ((number? datum) datum) 
         ((pair? datum) (car datum)) 
         (else (error "Wrong datum -- TYPE-TAG" datum))))

(define (attach-tag tag content)
  (cons tag content))

(define (contents datum) 
   (cond ((number? datum) datum) 
         ((pair? datum) (cdr datum)) 
         (else (error "Wrong datum -- CONTENTS" datum)))) 
  

(define (install-scheme-number-package)

  (define (tag x)
    ;attach-tag
    (attach-tag 'scheme-number x))
  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (install-real-package)

  (define (tag x)
    ;attach-tag
    (attach-tag 'real x))
  
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
  'done)

(define (install-rational-package)
;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y) )
                 (* (numer y) (denom x) ) )
              (* (denom x) (denom y) ) ) )
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y) )
              (* (denom x) (denom y) ) ) )
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y) )
              (* (denom x) (numer y) ) ) )
;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x) )

  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'make-rat '(rational) make-rat)
  
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y) ) ) )
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y) ) ) )
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y) ) ) )
  (put 'div '(rational rat ional)
       (lambda (x y) (tag (div-rat x y) ) ) )
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d) ) ) )
  'done)


(define (install-rectangular-package)
;; internal procedures
  (define (real-part z) (car z) )
  (define (imag-part z) (cdr z) )
  (define (make-from-real-imag x y) (cons x y) )
  (define (magnitude z)
    (sqrt (+ (square (real-part z) )
             (square (imag-part z) ) ) ) )
  (define (angle z)
    (atan (imag-part z) (real-part z) ) )
  (define (make-from-mag-ang r a)
    (cons (* r (cos a) ) (* r (sin a) ) ) )
;; interface to the rest of the system
  (define (tag x) (attach-tag ' rectangular x) )
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part )
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y) ) ) )
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a) ) ) )
  'done)

(define (install-polar-package)
;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
         (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
(lambda (x y) (tag (make-from-real-imag x y))))
  (put ' make-from-mag-ang ' polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
'done)


(define (install-complex-package)

;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a) )
;; internal procedures
  (define (add-complex zl z2)
    (make-from-real-imag (+ (real-part zl) (real-part z2) )
                         (+ (imag-part zl) (imag-part z2) ) ) )
  (define (sub-complex zl z2)
    (make-from-real-imag (- (real-part zl) (real-part z2) )
                         (- (imag-part zl) (imag-part z2) ) ) )
  (define (mul-complex zl z2)
    (make-from-mag-ang (* (magnitude zl) (magnitude z2) )
                       (+ (angle zl ) (angle z2) ) ) )
  (define (div-complex zl z2)
    (make-from-mag-ang (/ (magnitude zl) (magnitude z2) )
                       (- (angle zl) (angle z2) ) ) )
;; inteiface to rest of the system
  (define (tag z) (attach-tag 'complex z) )
  (put 'add '(complex complex)
       (lambda (zl z2) (tag (add-complex zl z2) ) ) )
  (put 'sub '(complex complex)
       (lambda (zl z2) (tag (sub-complex zl z2) ) ) )
  (put 'mul '(complex complex)
       (lambda (zl z2) (tag (mul-complex zl z2) ) ) )
  (put 'div '(complex complex)
       (lambda (zl z2) (tag (div-complex zl z2) ) ) )
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y) ) ) )
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a) ) ) )

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-real-package)

(define (square x) (* x x))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
  (let ((proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error
         "No method for these types -- APPLY-GENERIC"
         (list op type-tags))))))



(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-rational n d)
((get 'make 'rational) n d) )


(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))




;---------------------

(define *coercion-table* (make-hash))

(define (put-coercion coercion type proc)
  (hash-set! *coercion-table* (list coercion type) proc))

(define (get-coercion coercion type)

  (hash-ref *coercion-table* (list coercion type) #f))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define (scheme-number->rational n)
(make-rational (contents n) 1))

(define (rational->real n)

  (attach-tag 'real (/ (numer n) (denom n)))
  )


(define (real->complex n)
  (make-complex-from-real-imag (contents n) 0))

;(put-coercion 'scheme-number 'complex scheme-number->complex)
(put-coercion 'scheme-number 'rational scheme-number->rational)
(put-coercion 'rational 'real rational->real)
(put-coercion 'real 'complex real->complex)

(put-coercion 'scheme-number 'scheme-number (lambda (x) x))
(put-coercion 'rational 'rational (lambda (x) x))

(put-coercion 'real 'real (lambda (x) x))
(put-coercion 'complex 'complex (lambda (x) x))


(define (raise from to x)
  ((get-coercion from to) x)
  )

(define q (make-scheme-number 445))
(define w (raise (type-tag q) 'rational q))
(define e (raise (type-tag w) 'real w))
(define r (raise (type-tag e) 'complex e))

;(display (list q w e r))
;(newline)
;(display (map type-tag (list (make-scheme-number 2) (make-rational 3 2))))


;----------------------


(define (apply-generic2 op . args) 
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


(define (apply-generic3 op . args)

  (define (convert args)
    

    (define (iter l t i)

      ;(display "<-")
      ;(display l)
      ;(display "->")
      ;(newline)
      
      (if (null? l) i (let ((from (type-tag (car l)))
                              (to (type-tag t))
                              )
                          (let ((coer (get-coercion from to)))
                            (cond
                                  (coer (iter (cdr l) t (append (list (coer (car l))) i)))
                                  ;(coer (cons (coer (car l)) (iter (cdr l) t)) )
                                  (else #f)
                            )
                            
                            
                          )
                        )
      )

      ;(display (list from to coer))


    )

    (define (through-l items)

      (display "+--")
      (display items)
      (display "--+")
      ;(newline)
      (if (not (iter items (car items) '())) (display "FNHJKDA") void)

      
      (if (null? items) #f (let ((result (iter items (car items) '())))

                             (cond ((pair? result) result)
                                   ((not result) (through-l (cdr items)))
                                   
                                   
                             )

                           )
      )
      
      ;(define r (iter items (car items)))
      
    )

    
    
    (display (through-l args))
    (through-l args)
  )
  
  (define re (convert args))
  ;(display re)

  (if (not re) #f void)
  
  (define t-re (type-tag (car re)))
  ;(display t-re)

  (define (opiter i q op t)
    ;(display (get op t))

    (if (null? q) i (opiter ((get op t) (contents i) (contents (car q))) (cdr q) op t))
    
    )


  (if (or (eq? op 'mul) (eq? op 'div))
      (opiter (car re) (cdr re) op (list t-re t-re))
      (opiter (car re) (cdr re) op (list t-re t-re)))
  
  )



(define z (make-scheme-number 1))
(define x (make-scheme-number 2))
(define c (make-scheme-number 3))
(define v (raise 'scheme-number 'rational z))
(define b (make-complex-from-real-imag 6 3))


(apply-generic3 'add z x c)
(apply-generic3 'mul z x c)

(apply-generic3 'add v x c)
(apply-generic3 'mul v x c)

(apply-generic3 'add b x c)
(apply-generic3 'mul b x c)

#|

Look, 2.82 works as intented, sortof. It does indeed the coercion of all items into the value of one
and if it manages to do it, it operates and returns the correct answer. Problem is that I cant figure
out how to make it return a #f value to the main stack in case in which i cannot find a valid coercion
For example, It has successfully coerced scheme-number to rational and then to complex, but if I pass
a scheme-number and a complex one, it will not be able to convert them directly (since this implementation
does not include the raise functionality, which would allow it to try to climb the coercion ladder from
scheme-number to complex, which will then allow it to operate successfully) the code itself will give an error,
because it could not find a get-coer. However, we want the program to finish in a non-destructive way, so we would like
to return a #f value. 

2.83 has been implemented above.

|#