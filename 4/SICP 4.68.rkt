#lang sicp
#|
;(rule (append-to-form x y z))
;(append-to-form (a b) (c d) ?z)

(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))
|#



(rule (reverse () ()))
(rule (reverse ?x ?y) 
               (and (append-to-form (?first) ?rest ?x) 
                    (append-to-form ?rev-rest (?first) ?y) 
                    (reverse ?rest ?rev-rest)))