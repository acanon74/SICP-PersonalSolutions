#lang sicp
#|
(define s (cons-stream 1 (add-streams s s)))

It is going take the value in car and multiply it by 2

1
1 1 = 2

2
2 2 = 4

4
4 4 = 8

8
8 8 = 16


This also could be seen as the power of 2. Giving 2^n where
n is the iterant.
|#

