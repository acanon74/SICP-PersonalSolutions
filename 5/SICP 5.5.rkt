#lang sicp
#|
Originally written on a piece of paper. It doesnt make sense here
without arrows and boxes, specially after the fact.

2 done   2 after1
2 after1 1 after2
2 after2 0 after3

val b n

1 2 2 = val 2 call to after2
2 2 1 = val 4 call to after1
4 2 0 = val 8 call to done

|#


#|
fib(5)

n old new

5 done after11
  5 4

4 after11 after12
  4 3

3 after12 after13
  3 2

2 immediate-answer
  val 2
  call to after13

after13
n to 3
continue to after12

3 3 2
  after21 after22
  val 2

2 immediate-answer
  val 2
  call to after21

after2
n to 2
val to 2
continue to done
val to 4
call to done

done


|#