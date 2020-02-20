## Introduction

The main goal of this assignment was to help familiarize myself with the Haskell language and the workflows around `cabal`, the package/builder manager.

In this assignment, I added features to the FORTH interpreter and to wrote extensive tests.

## Installing packages

Make sure you are inside the FORTH directory
```
cabal install; cabal install hbase
```
## Running the unit tests

The easiest way to run the unit HSpec tests is using `runhaskell` command
```
runhaskell ValSpec.hs
runhaskell EvalSpec.hs
runhaskell InterpretSpec.hs
```

## To compile the existing code do:
```
cabal build
```

## To run the code:
```
cabal run tests/t1.4TH
cabal run tests/t2.4TH
cabal run tests/t3.4TH
...
cabal run tests/t*.4TH
```
Output files are in the form t*.out

## Observations
Some of the things I noticed while doing this project were that subtract, divide, power, and concat were impacted
by the stack order, and had to be written in "reverse" order to get the right calculation.

I also encountered issues with STR because if you pass in a type that can be interpreted as multiple types, it will
show the type along with the value. To fix this, I had to write separate rules for each individual type (Id, Real, Integer).

Moreover, I found importing Control.Monad (unless) useful to check if the stack still had items at the end of the operations,
and printed the remaining stack items inside this control statement
