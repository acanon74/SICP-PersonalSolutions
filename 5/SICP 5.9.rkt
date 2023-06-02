#lang sicp




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