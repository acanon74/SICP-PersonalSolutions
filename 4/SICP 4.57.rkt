#lang sicp

;(can-do-job (computer wizard) (computer programmer))
;(can-do-job (computer wizard) (computer technician))

(rule (same ?x ?x))

(rule (replace ?p1 ?p2)

      (and (job ?p1 ?job-candidate)
           (or (job ?p2 ?job-candidate)
               (and (job ?p2 ?job-replace)
                    (can-do-job ?job-candidate ?job-replace)))
           (not (same ?p1 ?p2))))

(replace ?people (Fect Cy D.))


(and (replace ?candidate ?person)
     (salary ?candidate ?salary-candidate)
     (salary ?person ?salary-person)
     (lisp-value > ?salary-person ?salary-candidate)
     )