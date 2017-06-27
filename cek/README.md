This project is an implementation of a basic CEK machine which interprets
a lambda expression. The lambda calculus constants are limited to integers.
A lambda term must evaluate to a constant for it to be accepted by the machine,
otherwise it will report a syntax error (although each step of the machine will
be shown so it may be useful for reductions).

To run the CEK machine, run the command:
  ghci cek.hs

This will open GHC's interactive environment.

From this environment, run the command:
  interpret exampleLambda

Here, interpret is the function in cek.hs that will interpret a lambda
expression and exampleLambda is just  a pre-defined lambda expression in cek.hs.
There is another pre-defined lambda expression exampleLambda2 which uses here
and go.

Custom lambda expressions can be defined using the data types in cek.hs.
