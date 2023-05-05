#lang racket
#|

Pal Luis is wrong.

This is not a problem since this is a uni linear problem.
If we manage to withdraw the money from a1 then we couldn't care
less about what other procedures do to a1 or a2. Regardless of all
of that we will deposit that amount into a2.

It is different to exchange because exchange requires multiple
reads and writes coming from and to both accounts, so once we read
the values and calculate the difference we rely on those values still
being true for the rest of the duration of the exchange procedure,
if these were to change mid execution then the procedure doesnt
not make sense.

|#