#lang sicp


(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell #f))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

(define (test-and-set2! cell n limit)

  (if (>= n limit) #t
      (begin (set-car! cell #t)
             #f))
  
  )

(define (clear2! cell n)

  (set-car! cell #f)
  (set! n (- n 1))
  )

(define (make-semaphore max)
  (let ((limit max)
        (n 0)
        (cell (list #f)))

    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set2! cell n limit)
                 (the-semaphore 'acquire)))
            ((eq? m 'release) (clear2! cell n))))
    the-semaphore))


(define (make-semaphore2 max)
  (let ((limit max)
        (n 0)
        (mutex (make-mutex)))

    (define (the-semaphore2 m)
      (cond ((eq? m 'acquire)
             (if (< n limit)
                 (begin (set! n (+ n 1))(mutex 'release))
                 (begin (mutex 'release) (the-semaphore2 'acquire))
                     )

             )
            
            ((eq? m 'release)
             (begin (mutex 'acquire)
                    (set! n (- n 1))
                    (mutex 'release)))))
    the-semaphore2))

