#lang sicp
#|


Generic operations:
You ought to write the whole implementation for every new data type and operations.
As well as how the codebase interacts with the new implementations.

Data-directed style:
You only need to add the new implementation and data type to the look up table.
It is not necessary to change how the codebase interacts with this, since this will be accomplish
by standarized procedures relying on the look up table.

Message-passing style:
You will have to implement the new types and operations, inside every new class.
However, it is not necessary to change the codebase since it will rely on the initial
implementations inside the data types.



|#
#|
Wiki because I am lazy :D

Adding New Types
To add new types one must create the constructor/selectors and update the cond/if blocks which handle dispatch. You have to jump from operation to operation to update the dispatch tables but only one constructor is altered (created).

Adding new Operations
Creating new operations does not require any code changes outside of the creation of the new operation. Might have to jump around to make sure you include each data type in the dispatch table.

Data-Directed Style
Adding New Types
Create and run an "installer" function. Installer should a. implement the constructor, selectors, and each operation b. install with the get procedure the function to the dispatch table.

This style only requires editing/calling the installer which can be done only editing one code location (no jumping :)). Although you might have to jump to your generic function section to remember all of the operations to implement.

Adding New Operations
Each operation dispatch case is written in a separate "installer" function. Adding a new operation involves creating a new table "row" (generic function) and each "column" (operation defined for a specific data type in installer). You have to create the new generic function, and edit each data type installer which requires the new operation. Lot of jumping :(

Message-Passing Style
Adding New Types
Message-passing style internalizes both data and operations completely. Creating a new data type requires only the creation of the constructor. The resulting object must implement internally all selectors, and operations. It is possible to add new types only editing one code location (no jumping).

Adding New Operations.
Because each operation implementation is internal to the object adding new operations requires editing each data type constructor which requires the new operation.



Recommendation: Frequent Type Additions
Message-passing and data-directed styles are equivalent optimal choices. Both internalize their operation implementations. Where operation additions grows slowly relative to type additions on average programmers will have to edit less code locations than direct-dispatch. In my experience this introduces less bugs/mistakes.

Recommendation: Frequent Operation Additions
Direct-dispatch localizes operation logic internal to its definition for all data types. If data types are not constantly being added the code blocks which dispatch on type will not endlessly grow, which can be hard to manage.



|#