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
  (let ((local-table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((equal? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    
    (define (lookup key . args)

      (define (iter keys items)
        (display items)
        (newline)
        (if (null? keys) (mcdr items)
            (let ((subtable (assoc (car keys) (mcdr items))))
              (if subtable
                  (iter (cdr keys) subtable)
                  #f)))
      )
      (iter (cons key args) local-table)
      
)

(define (insert! value key1 key2)
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
    

    (define (insert!2 value key . args)


      (define (create-mcons keys value)

        (if (null? keys) value (mcons (car keys) (create-mcons (cdr keys) value)))
        )
      
      (define (iter keys items)
        (display items)
  
        (let ((subtable (assoc (car keys) (mcdr items))))
          (display subtable)
          (newline)
          (if subtable
              (if (null? (cdr keys)) (set-cdr! subtable value)
                  (iter (cdr keys) subtable))
              (set-cdr! items
                        (mcons (mlist (car keys) (create-mcons (cdr keys) value)) (mcdr items))))

          )

        )
      (iter (cons key args) local-table)
      )


    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'display) local-table)
            ((eq? m 'new) insert!2)
            (else (error "Error while dispatching! -- TABLE" m))))
    dispatch))

(define t (make-ltable))
(define get (t 'lookup-proc))
(define put (t 'insert-proc!))
(define table (t 'display))

(put 1 'q 'a)
(display table)
(put 2 'q 'b)
(put (mcons (mcons 't 44) '()) 'q 'a)

(get 'q 'a 't)
(display table)

(display "\nNEWNEW\n")

(define p (t 'new))

(p 888 'w 'e 'r)

(p 4 'q 'b)

(newline)
(display ">>")
(display table)
(newline)

;(put 66 'q 'a 't)
(newline)
(display ">>")

#|

Current implementation does not work, it break if we try to insert a value in an index with a subtree. Although the wiki says it should
support it, I find it implementation-dependent, since one might not want to have unindexed values in a given key.
|#

