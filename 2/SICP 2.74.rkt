#lang racket

(define first '(div1 (adam (100 av1)) (emma (200 av2))))

(define second '(div2 (employees (ivan 300 av3) (anna 400 av4)) (taxes 1 2)))


(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))


(define (install-div1)
  ;'(div1 (adam (100 av1)) (emma (200 av2)))
  (define (get-employees file) (cdr file))
  (define (get-account employees) (car employees))
  (define (get-name account) (car account))
  (define (get-salary account) (caadr account))
  (define (get-address account) (cadadr account))

  
  (define (get-keys file)
    (define (iter f)
      (if (null? f) '() (cons (get-name (get-account f)) (iter (cdr f))))
      )
    (iter (get-employees file))
    
    )


  (define (retrieve file key)

    (define (iter f key)
      (cond ((null? f) #f)
            ((eq? (get-name (get-account f)) key) (get-account f))
            (else (iter (cdr f) key))

            )
      
      )
    (iter (get-employees file) key)
    )
  

  (put 'get-account 'div1 get-account)
  (put 'get-name 'div1 get-name)
  (put 'get-salary 'div1 get-salary)
  (put 'get-address 'div1 get-address)
  (put 'get-keys 'div1 get-keys)
  (put 'retrieve 'div1 retrieve)
  )

(define (install-div2)
  ;'(div2 (employees (ivan 300 av3) (anna 400 av4)) (taxes 1 2))
  
  (define (get-employees file) (cdadr file))
  (define (get-account employees) (car employees))
  (define (get-name account) (car account))
  (define (get-salary account) (cadr account))
  (define (get-address account) (caddr account))
  
  (define (get-keys file)
    (define (iter f)
      (if (null? f) '() (cons (get-name (get-account f)) (iter (cdr f))))
      )
    (iter (get-employees file))

    )

  (define (retrieve file key)

    (define (iter f key)
      (cond ((null? f) #f)
            ((eq? (get-name (get-account f)) key) (get-account f))
            (else (iter (cdr f) key))

            )
      
      )
    (iter (get-employees file) key)
    )

  (put 'get-account 'div2 get-account)
  (put 'get-name 'div2 get-name)
  (put 'get-salary 'div2 get-salary)
  (put 'get-address 'div2 get-address)
  (put 'get-keys 'div2 get-keys)
  (put 'retrieve 'div2 retrieve)
  )


(install-div1)
(install-div2)

(define (get-division file)

  (car file)
  )

(define (find-employee-record file key)

  ;this is global to make it system-wide. If not, we would have to define a retrieve a
  ;record-constructor for every division and that doesnt make sense if the objective is
  ;make it a standard
  (define division (get-division file))
  (define account ((get 'retrieve division) file key))

  (define name ((get 'get-name division) account))
  (define salary ((get 'get-salary division) account))
  (define address ((get 'get-address division) account))
  
  (list division name salary address)
  )


(define (get-salary file key)

  (define division (get-division file))

  ((get 'get-salary division) ((get 'retrieve division) file key))
  
  )

(display ((get 'get-keys 'div1) first))
(display ((get 'get-keys 'div2) second))
(display ((get 'retrieve 'div1) first 'adam))
(display ((get 'retrieve 'div2) second 'anna))

;adam emma ivan anna
(find-employee-record first 'adam)
(find-employee-record second 'anna)
(get-salary first 'adam)
(get-salary second 'ivan)



;They will have to provide their file, tag it accordingly with the division identifier,
;And provide implementations for the install-divX function. After that, everything will be
;standarized and co-exists with the rest of the system and divisions.