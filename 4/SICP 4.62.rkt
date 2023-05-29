#lang sicp

#|
;(rule (append-to-form x y z))
;(append-to-form (a b) (c d) ?z)

(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))
|#




(rule (last-pair (?x) (?x)))


(rule (last-pair (?y . ?r) (?x))
      (last-pair ?r (?x)))