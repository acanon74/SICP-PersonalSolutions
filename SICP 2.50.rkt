#lang sicp
(#%require sicp-pict)
(define e (flip-horiz einstein))


(define (new-flip-horiz painter)

  (transform-painter painter (make-vect 1 0) (make-vect 0 0) (make-vect 1 1))
  )


(define (new-flip-vert painter)

  (transform-painter painter (make-vect 1 1) (make-vect 0 1) (make-vect 1 0))
  )

(define (new-below p1 p2)


  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-top
           (transform-painter p1
                              split-point
                              (make-vect 1 0.5)
                              
                              (make-vect 0 1)))
          (paint-bottom
           (transform-painter p2
                              
                              (make-vect 0 0)
                             
                              (make-vect 1 0)
                              split-point)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame))))
  
  )

(define (new-flip-270 painter)

  (transform-painter painter (make-vect 0 1) (make-vect 0 0) (make-vect 1 1))
  )

(define (new-new-below p1 p2)

  (new-flip-vert (new-flip-270 (beside (new-flip-270 p1) (new-flip-270 p2))))

  )
(paint (below e e))
(paint (new-new-below e e))