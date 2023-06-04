#lang racket
Left to right inherit from our list interpretation procedures

It does given the nature of the stack, we have seen that
we have to reverse the order in which we compile and append
instructions of a sequence due to the fact that later on
we have to unstack everything and, wihout reversing first, we
would get a reverse list of paramaters.
If we were to change the compiler to compiler from right
to left, this will no longer generate a problem in sequences,
however, it will likely be a problem in other section of the
compiler such as compilation of the instructions, etc.