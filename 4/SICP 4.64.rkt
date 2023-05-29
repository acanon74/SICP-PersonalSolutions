#lang sicp
;Original code
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

;Louis code
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

#|

(and (outranked-by ?middle-manager ?who)
     (supervisor (Bitdiddle Ben) ?middle-manager))


The first clause in the and operation will result in an
endless loop. This is due to the fact that both
 parameters are unbounded variables and so, we are asking whether
there is anyone outranked by anyone. This assertion is true in
multiple input streams, however, further execution of the
outranked-by will use this unbounded variables, indefinetely
checking for anyone outranked by anyone and the second clause
supervisor will never be executed.

WIKI:

 first,  query (outranked-by (Bitdiddle Ben) ?who),
after unifying the conclusion of rule.
we will evaluate (outranked-by ?middle-manager ?boss),
this will query (outranked-by ?staff-person ?boss) again,
so it will be in infinite loop. 

|#