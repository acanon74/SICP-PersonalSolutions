#lang sicp
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))


(rule (big-shot? ?person ?division)
      
      (and (job ?person (?division . ?title))
           
           (or (not (supervisor ?person ?supervisor))
               (and (supervisor ?person ?supervisor)
                    (not (job ?supervisor (?division . ?title2)))
                    (not (big-shot? ?supervisor ?division)))
               
               )

           )
      )