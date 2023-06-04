#lang sicp
(f 'x 'y) -> all are superfluous
((f) 'x 'y) -> all are superfluous
(f (g 'x) y) -> we will need to save&restore proc and argl so that we can evaluate g with parameters 'x and then return to
evaluate f with the result and y. We also need to save&restore env so that we can lookup the value of y (since it is a variable not
                                                                                                               symbol) in the correct
env corresponding to the evaluation of the whole procedure and not only of g.
(f (g 'x) 'y) -> only argl and proc. Notice that 'y is a symbol and therefore evaluating on env of g will not change its value.