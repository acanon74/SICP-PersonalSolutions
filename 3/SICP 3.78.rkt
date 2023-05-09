#lang sicp

(define (stream-ref s n)
  (display s)
  (newline)

  (if (= n 0)
      
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

#|
(cons-stream a b)
 (cons a (delay b))

(delay exp)
 (lambda () exp)

|#


(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
#|
(define (force delayed-obj)
  (delayed-obj))
|#

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #f)
                 result)
          result))))
(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map2 + s1 s2))

(define (mul-streams s1 s2)
  (stream-map2 * s1 s2))

(define factorials (cons-stream 1 (mul-streams integers factorials)))


(define (show x) (display-line x) x)

;(define x (stream-map show (stream-enumerate-interval 0 10)))


(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))


(define (partial-sums s)
  (define t (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))
  t)

;(define a (stream-map show (partial-sums integers)))

;(stream-ref a 10)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (square x) (* x x))


(define (stream-map2 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map2
              (cons proc (map stream-cdr argstreams))))))

#|
(define (integral delayed initial-value dt)

  (define init
    (cons-stream initial-value
                 (let ((integrand (force delayed)))
                   (if (stream-null? integrand)
                       the-empty-stream
                       (integral (delay (stream-cdr integrand))
                                 (+ (* dt (stream-car integrand))
                                    initial-value)
                                 dt)))))
  init)

|#


(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((intgrand (force delayed-integrand)))
                   (add-streams (scale-stream delayed-integrand dt)
                                int))))
  int)



(define (solve1 f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)



(define (solve-2nd f dt y0 dy0)

  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream ddy b)))
  y)

(stream-ref (solve-2nd 1 0 0.0001 1 1) 10000)  ; e                                                         
 (stream-ref (solve-2nd 0 -1 0.0001 1 0) 10472)  ; cos pi/3 = 0.5                                           
 (stream-ref (solve-2nd 0 -1 0.0001 0 1) 5236)  ; sin pi/6 = 0.5 
