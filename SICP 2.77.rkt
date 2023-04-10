#lang sicp
#|

The object z has the tag complex. One the procedure in magnitude tries to key
the selectors of an expected rectangular or polar object, it will instead key by
the complex selector, which will return the general selectors for the data, which
will get the contents of the rectangular or polar tag, and operate on that data.


Before:

get tried key "complex", for which selector dont exist

Now:

get tries key "complex", which returns the generic selector, which work with rectangular
or polar objects.

|#

