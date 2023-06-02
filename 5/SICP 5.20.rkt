#lang sicp
#|

The wiki does not terminate the list/vectors with empty characters even though the book does? Any way, here
is my solution.

x - (1,2)
y - (x,x) - ((1,2),(1,2))

--> 1(_ , =) --> 2(_ , \)
      |            |
      |-------------
      |
    3(_ , =) --> 4(_ , \)
      |            | 
      |            |
      1            2


index 0 1 2 3 4
cars p3 p3 n1 n2
cons p2 e0 p4 e0

The pointers that hold the values of is
x - p3
y - p1

The final value of free is
free - p2
|#