#lang sicp
;Someone is a wheel if he supervises someone who is in turn
;a supervisor

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

#|

The way wheel is written makes so that a given person is a
wheel for EVERY person he is a supervisor (directly or not) of.


WIKI:


That's because there are four middle-manager
whose manager is Warbucks Oliver. 

To add more detail:

Because Warbucks supervises:

1. Scrooge who supvervises Cratchet

2. Bitdiddle who supervises Hacker

3. Bitdiddle who supervises Fect

4. Bitdiddle who supervises Tweakit

Each path is taken and is reported as a separate case.



|#