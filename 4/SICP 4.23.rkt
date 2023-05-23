#lang sicp
#|

In the books version the procedure builds the sequence so
that it is executed in a sequential order, that is it returns
and object of the form

(lambda () ((lambda () ((lambda () (exp1 exp2)) exp3)) exp4))

Also, this procedure only analyzes the expression.

Alyssas on the other hand, evaluates the expression in
compiling time and does so directly, that is of the form


(list exp1 exp2 exp3)

Where she runs every expression independently. Moreover, she
executes the expression so this breaks the analysis abstraction
wall we had built.


|# 