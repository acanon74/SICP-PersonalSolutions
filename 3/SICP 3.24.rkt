#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)


(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (assoc-param key records parameter?)
  (cond ((null? records) #f)
        ((parameter? key (mcar (mcar records))) (mcar records))
        (else (assoc-param key (mcdr records) parameter?))))


(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key table) (cdr table)))))
  'ok)


(define (make-table)
  (list '*table*))

(define (lookup2 key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert!2 key1 key2 value table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key2 value)
                              (cdr subtable)))))
          (set-cdr! table
                    (cons (list key1
                                (cons key2 value))
                          (cdr table)))))
        'ok)

(define (make-ltable)
  (let ((local-table (list '*table*)))
    (define (lookup key1 key2)
      (let ((subtable (assoc key1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key1 key2 value)
      (let ((subtable (assoc key1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key2 (mcdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (mcons (mcons key2 value)
                                  (mcdr subtable)))))
            (set-cdr! local-table
                      (mcons (mlist key1
                                  (mcons key2 value))
                            (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "You fucked up buddy! -- TABLE" m))))
    dispatch))

(define (make-pltable same-key?)
  (let ((local-table (mlist '*table*)))
    (define (ilookup key1 key2)
      (let ((subtable (assoc-param key1 (mcdr local-table) same-key?)))
        (if subtable
            (let ((record (assoc-param key2 (mcdr subtable) same-key?)))
              (if record
                  (mcdr record)
                  55))
            #f)))
    (define (iinsert! key1 key2 value)
      (let ((subtable (assoc key1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key2 (mcdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (mcons (mcons key2 value)
                                  (mcdr subtable)))))
            (set-cdr! local-table
                      (mcons (mlist key1
                                  (mcons key2 value))
                            (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) ilookup)
            ((eq? m 'insert-proc!) iinsert!)
            ((eq? m 'd) local-table)
            (else (error "Error while dispatching! -- TABLE" m))))
    dispatch))


(define (test? key value)
  (if (equal? key value) #t (even? value)))

(define (near? key value)
  (if (equal? key value) #t (<= (abs (- value key)) 0.5))
  )

(define t (make-pltable near?))

(define get (t 'lookup-proc))
(define put (t 'insert-proc!))
(define p (t 'd))

(put 'number 1 'q)
(put 'number 5 'w)
(put 'number 6 'e)
(put 'number 2 'gg)
(put 'number 2.1 'r)
(put 'number 2.4 't)
(put 'number 3 'y)


(get 'number 3)
(get 'number 2)
(put 'letter '00 '000)


(display p)
