#lang sicp


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))



(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))




;////////EX 2.67////////////



(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))


;(define sample-set (list (list 'A 4) (list 'B 2) (list 'D 1) (list 'C 1)))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ;(A D A B B C A)
(define sample-encode '(A D A B B C A))

;///////EX 2.68///////////////


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


(define (element-of-set? x set)
  
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;symbols, weight, left-branch, right

;leaf? symbol, weight-leaf

(define (encode-symbol symbol tree)
  (cond   ((not (element-of-set? symbol (symbols tree))) (error "SYMBOL NOT FOUND!"))
          ((and (leaf? tree) (eq? (symbol-leaf tree) symbol)) '())
          ((element-of-set? symbol (symbols (left-branch tree)))  (cons 0 (encode-symbol symbol (left-branch tree))))
          ((element-of-set? symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
          (else (error "Something went really wrong! -- encode-symbol"))))

(encode sample-encode sample-tree) ;(0 1 1 0 0 1 0 1 0 1 1 1 0)


;////////////EX 2.69///////////////////

(display "2.69******\n")





(define test (list (list 'C 1) (list 'B 3) (list 'A 8) (list 'D 1) (list 'E 1) (list 'F 1) (list 'G 1) (list 'H 1) ))



(define (successive-merge leafs)

  
  (define (iter pairs m1 m2 b)

    (cond ((null? pairs) (successive-merge (cons (make-code-tree m2 m1) b)))
          ((< (weight (car pairs)) (weight m1)) (iter (cdr pairs) (car pairs) m2 (cons m1 b)))
          ((< (weight (car pairs)) (weight m2)) (iter (cdr pairs) m1 (car pairs) (cons m2 b)))
          (else (iter (cdr pairs) m1 m2 (cons (car pairs) b) ))
          )
    
    )


  
  (if (= (length leafs) 1) (car leafs) (iter (cddr leafs) (car leafs) (cadr leafs) '()))
  
  
  

)

;(successive-merge (make-leaf-set test))


;(define sample-tree
;  (make-code-tree (make-leaf 'A 4)
;                  (make-code-tree
;                   (make-leaf 'B 2)
;                   (make-code-tree (make-leaf 'D 1)
;                                   (make-leaf 'C 1)))))



(define sample-set (list (list 'A 4) (list 'B 2) (list 'D 1) (list 'C 1)))

;(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(decode sample-message sample-tree) ;(A D A B B C A)
;(define sample-encode '(A D A B B C A))
;(display sample-tree)
;(right-branch (car (successive-merge (make-leaf-set sample-set))))
(successive-merge (make-leaf-set sample-set))
(decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) (successive-merge (make-leaf-set sample-set)))


(define (generate-huffman-tree pairs)
 (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree sample-set)


;////////////Ex 2.70/////////////////////


(define rock-set (list (list 'A 2)
                       (list 'BOOM 1)
                       (list 'GET 2)
                       (list 'JOB 2)
                       (list 'NA 16)
                       (list 'SHA 3)
                       (list 'YIP 9)
                       (list 'WAH 1)))

(define rock-message '(GET A JOB
                       SHA NA NA NA NA NA NA NA NA
                       GET A JOB
                       SHA NA NA NA NA NA NA NA NA
                       WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                       SHA BOOM))

(generate-huffman-tree rock-set)


(define encoded (encode rock-message (generate-huffman-tree rock-set)))

(decode encoded (generate-huffman-tree rock-set))



;//////////Ex 2.71//////////////////

; Most frequent: 1 bit. That is at most 1 bit for the  symbol with weight 2^n-1
; Least frequent: n-1 bits. That is at least n-1 for first 2 symbols with weight 2^1 and 2^2

;/////////Ex 2.72///////////////////

;Best O(1)
;Worst O(n)