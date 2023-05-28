#lang sicp


(and (supervisor ?name (Bitdiddle Ben))
     (address ?name ?address))

(and (salary (Bitdiddle Ben) ?ben-salary)
     (salary ?person ?amount)
     (lisp-value < ?amount ?ben-salary))

(and (supervisor ?person ?supervisor)
     (not (job ?supervisor (computer . ?title)))
     (job ?supervisor ?job-supervisor))