#lang sicp
start
(goto (label here) )
here
(assign a ( const 3) )
(goto (label there) )
here
(assign a (const 4) )
(goto (label there) )
there


#|
a will be 3.

(define m (make-machine '(a) '()


                        '(
                          start
                          (goto (label here))
                          here
                          (assign a (const 3))
                          (goto (label there))
                          here
                          (assign a (const 4))
                          (goto (label there))
                          there)
                        ))

(set-register-contents! m 'a 1)
(start m)
(get-register-contents m 'a)

|#
(define (exist? label labels)
  (not (assoc label labels))
  )

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (if (exist? next-inst labels)
                                  (receive insts
                                       (cons (make-label-entry next-inst
                                                               insts)
                                             labels))
                                  (error "Duplicated label in controller -- EXTRACT-LABELS" next-inst))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))