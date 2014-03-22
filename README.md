# h2048

a haskell implementation of "2048"

Based on [2048](https://github.com/gabrielecirulli/2048)

## Build and run

### With cabal

If you have [Cabal](http://www.haskell.org/cabal/) installed,
you can use the following command to build this project:

    cabal build

The executable will be located at `dist/build/h2048-simple/h2048-simple`,
to run the program:

    ./dist/build/h2048-simple/h2048-simple

Or alternatively:

    cabal run h2048-simple

### Without cabal

First make sure the following dependencies are installed:

* [transformers](http://hackage.haskell.org/package/transformers)
* [mtl](http://hackage.haskell.org/package/mtl)
* [MonadRandom](http://hackage.haskell.org/package/MonadRandom)

You can use following commands to run this project without cabal:

    cd src # assume your working directory is the project home.
    runhaskell MainSimple.hs

## How to play

Run command `runhaskell h2048.hs` from your terminal.

keys:

* `q`: quit
* `i`: up
* `k`: down
* `j`: left
* `l`: right
