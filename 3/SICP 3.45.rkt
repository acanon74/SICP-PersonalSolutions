#lang racket
#|

In this approach if we had two sequencial calls to exchange-serialized
we would put the exchange procedure in a serializer, and
inside it another serializer for the withdraw and deposit procecures.
Therefore withdraw and deposit calls from both calls of exchange
would be able to run concurrently, even though the exchange procedures
are actually running sequencially in their own serializer.

I am wrong, here is the wiki.

The correct implementation of serialized-exchange in the book
deliberately leaves the dispatched withdraw and deposit
procedures *un*-serialized, so that when the "raw" exchange
procedure calls them and then gets wrapped, via serialized-exchange,
 in the serializers of both accounts involved in the exchange,
 there will be no conflict.

Louis' proposed change would be disastrous because the dispatched
 withdraw and deposit procedures called in the "raw" exchange
procedure would *already* have serializers, so when
 serialized-exchange wraps the raw exchange in both serializers
 *again*, it wouldn't even be possible to perform the required
 withdraws and deposits, *by definition*, since two procedures
can be run concurrently if and only if they have *not*
 been serialized with the same serializer.


TLDR: Then the deposit and withdraw procedure would be in their
own serializer, which would make it impossible for the serializer
of the exchange procedure to interact with them and run them
sequentially.

|#