#lang sicp
(#%require sicp-pict)


(define (make-vect x y)

  (cons x (cons y nil))
  )

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (car (cdr vect)))

(define t1 (make-vect 1 2))
(define t2 (make-vect 3 4))

(define (neg-vect vect)
  (scale-vect vect -1))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (let ((neg-v2 (neg-vect v2)))
    (add-vect v1 neg-v2)))

(define (scale-vect vect scalar)
  (make-vect (* (xcor-vect vect) scalar) (* (ycor-vect vect) scalar)))

;;(neg-vect t2)
;;(add-vect t1 t2)
;;(sub-vect t1 t2)
;;(scale-vect t1 4)


(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))


(define (get-origin1 frame)
  (car frame))

(define (get-edge11 frame)
  (cadr frame))

(define (get-edge21 frame)
  (caddr frame))

(define t3 (list 1 2 3))

(get-origin1 t3)
(get-edge11 t3)
(get-edge21 t3)

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (get-origin2 frame)
  (car frame))

(define (get-edge12 frame)
  (cadr frame))


(define (get-edge22 frame)
  (cddr frame))

(define t4 (cons 1 (cons 2 3)))

(get-origin2 t4)
(get-edge12 t4)
(get-edge22 t4)


;;(define (segments->painter segment-list)
  ;;(lambda (frame)
    ;;(for-each
     ;;(lambda (segment)
       ;;(draw-line
        ;;((frame-coord-map frame) (start-segment segment))
        ;;((frame-coord-map frame) (end-segment segment))))
     ;;segment-list)))

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define seg (make-segment (make-vect 1 2) (make-vect 3 4)))
(start-segment seg)
(end-segment seg)
