#lang sicp

#|

a: This breaks the nature of the raise or lower procedures. Instead of trying to raise or
lower the types to apply op, it will default to return #f if the requested op is not
readily available for the initial types.

Without his modifications we would expected the procedure generic to lower (if possible)
the type of the complex number up to scheme-numbers, and perform op based on them. However,
with the changes this will never occur.

b:

He was incorrect, because generic always tries to apply op with the initial types.
It is only if this is not possible that it will move types through the tree.
So it is never going to convert anything to its type.

c:


(if (= type1 type2) (error "SAME TYPE"))

|#

#|

a: For two types scheme-number, it works in the first generic call.
For the two complex numbers, it will do coercion but fail anyways after the 2 last generic
calls, after it has tried to convert types, unsuccesfully since it will convert to the same.

Wiki: apparently it doesnt exist after the 2 last generic calls, it goes
into infinite recursion given the implementation of the apply-generic procedure

b:

He was correct that indeed, coercion might be tried in same-type parameters and break things, however,
the program would still behave as expected since it would fail regarless of his
changes after the failed coercion and failed generic calls.

c:

A simple if before the coercion that triggers and error if the types are the same and
op could not be found initially.

|#