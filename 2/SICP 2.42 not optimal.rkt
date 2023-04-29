#lang racket/base

(define (filter predicate sequence) 
   (cond ((null? sequence) '()) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence))))) 

(define (accumulate op initial sequence)

  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high) 
   (if (> low high) 
       '()
       (cons low (enumerate-interval (+ low 1) high)))) 

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (generate-row length default pos item i)


  (cond ((> i length) '())
        ((= i pos) (cons item (generate-row length default pos item (+ i 1))))
        (else (cons default (generate-row length default pos item (+ i 1)))))
)


(define (generate-board length height)
  
  (cond ((= height 0) '())
        (else (cons (generate-row length 0 1 0 1) (generate-board length (- height 1)))))
)


(define (get-y-in-matrix m key i)

  (cond
        ((= i key) (car m))
        (else (get-y-in-matrix (cdr m) key (+ i 1))))
)


(define (queens board-size)

  (define empty-board '())
  
  (define (adjoin-position x-pos k current)
    (append current (list (generate-row board-size 0 x-pos 1 1)))
  )

  
  (define (safe? k current)

    (define (find-x-in-list items key i)
      (if (= (car items) key) i (find-x-in-list (cdr items) key (+ i 1)))
    )
  
    (define (get-coordinates board k)
      (map (lambda (col) (append (list (find-x-in-list (get-y-in-matrix board col 1) 1 1)) (list col))) (enumerate-interval 1 k))
    )

    (define queens-coords (get-coordinates current (- k 1)))
    (define row-checking (get-y-in-matrix current k 1))
    (define pos-checking (list (find-x-in-list row-checking 1 1) k))

    
    (define (validate-x forbidden x k)

      (cond ((null? forbidden) #t)
            ((= x (car (car forbidden))) #f)
            (else (validate-x (cdr forbidden) x k))
      )

    )
    

    (define (validate-diag forbidden x-checking y-checking k i)
      
      (define (iter x-forbidden y-forbidden x-checking y-checking k j)

        (cond ((> j k) #t)
              ((and (= x-checking (+ x-forbidden j)) (= y-checking (+ y-forbidden j))) #f)
              ((and (= x-checking (- x-forbidden j)) (= y-checking (+ y-forbidden j))) #f)
              (else (iter x-forbidden y-forbidden x-checking y-checking k (+ j 1)))
        )

      )

      (cond ((null? forbidden) #t)
            ((not (iter (car (car forbidden)) (car (cdr (car forbidden))) x-checking y-checking k 1)) #f)
            (else (validate-diag (cdr forbidden) x-checking y-checking k (+ i 1)))

      )


    )

    (cond ((not (validate-x queens-coords (car pos-checking) k)) #f)
          ((not (validate-diag queens-coords (car pos-checking) (car (cdr pos-checking)) k 1)) #f)
          (else #t)

          )
  
)


  (define (queen-cols k)
    
    (if (= k 0) (list empty-board)
        
        (filter (lambda (positions) (safe? k positions))
                
                (flatmap
                 (lambda (rest-of-queens)
                   (map (lambda (new-row)
                          
                          (adjoin-position new-row k rest-of-queens))
                        (enumerate-interval 1 board-size)))

                 (queen-cols (- k 1)))
        )
    )

  )
  (queen-cols board-size)


  
  )

(define given (list (list 0 0 0 0 0 1 0 0)
                    (list 0 0 1 0 0 0 0 0)
                    (list 1 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 1 0)
                    (list 0 0 0 0 1 0 0 0)
                    (list 0 0 0 0 0 0 0 1)
                    (list 0 1 0 0 0 0 0 0)
                    (list 0 0 0 1 0 0 0 0)))


(queens 4)