#lang sicp

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))


(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                           result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
    
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))


(define fig2-16-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))) 
(define fig2-16-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ()))))) 


(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)

  (cond ((or (null? set) (> (car set) x)) (cons x set))
        ((= (car set) x) set)
        ((< (car set) x) (cons (car set) (adjoin-set x (cdr set))))
        
        )
  )

;(define (lookup given-key set-of-records)
 ; (cond ((null? set-of-records) #f)
  ;      ((equal? given-key (key (car set-of-records)))
   ;      (car set-of-records))
    ;    (else (lookup given-key (cdr set-of-records)))))

(define t '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))


(define (key x) (entry x))


(define (lookup given-key items)

  (cond ((null? items) #f)
        ((equal? given-key (key items)) )
        ((< given-key (key items)) (lookup given-key (left-branch items)))
        ((> given-key (key items)) (lookup given-key (right-branch items)) )

        )
  
  )

(lookup 7 t)
(lookup 99 t)