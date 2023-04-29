#lang sicp
(#%require sicp-pict)

(define e (flip-horiz einstein))
(define m mark-of-zorro)

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (right-split e 1))
(paint (up-split e 1))

(define (split op op2)

  (lambda (painter) (op painter (op2 painter painter)))

  )

(define right-split2 (split beside below))
(define up-split2 (split below beside))
(paint (right-split2 e))
(paint (up-split2 e))