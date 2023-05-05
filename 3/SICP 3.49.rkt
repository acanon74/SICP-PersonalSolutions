#lang sicp
#|

From the wiki

It wouldn't work for procedures that operate on a shared resource before determining some otherly shared resource to operate on.

Suppose accounts have an internal list of pairs consisting of an account and an amount and we invent a procedure that can take one account, and look at the first pair in its list and make a deposit accordingly.

Than if account A has account B first in its list and vice versa, concurrently calling our invented procedure on both could result in a deadlock.



|# 