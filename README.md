# h2048

[![Build Status](https://travis-ci.org/Javran/h2048.svg?branch=master)](https://travis-ci.org/Javran/h2048)

A haskell implementation of Game 2048. Including:

* a library for experimenting game strategies for Game 2048
* a simple CLI that merely pretty-prints the game board
* a better CLI implemented using [vty-ui](http://hackage.haskell.org/package/vty-ui)

Based on [2048](https://github.com/gabrielecirulli/2048)

# Screenshots

## Simple CLI version

![](https://github.com/Javran/h2048/releases/download/0.1.0.0/h2048-simple.jpg)

## vty CLI version

![](https://github.com/Javran/h2048/releases/download/0.1.0.0/h2048-vty.jpg)

## Build and run

### With cabal

`h2048` is now available on [hackage](http://hackage.haskell.org/package/h2048).

If you have [Cabal](http://www.haskell.org/cabal/) installed,
you can use the following command to install this project:

    cabal update
    cabal install h2048

The binaries are `h2048-simple` for simple CLI version, `h2048-vty` for CLI version
implemented using `vty-ui`.

### Flags

If you just want the functionality of this library, you can turn off flag `exe`.
If you have trouble building the `vty` CLI version, you can try to turn off flag `vty`.

An example for turning off flag `vty`:

    # if you are installing package from hackage:
    cabal install --flag="-vty"

    # or if you are building from the github repo
    cabal configure --flag="-vty"
    cabal build

### Without cabal

First make sure the following dependencies are installed:

* [transformers](http://hackage.haskell.org/package/transformers)
* [mtl](http://hackage.haskell.org/package/mtl)
* [MonadRandom](http://hackage.haskell.org/package/MonadRandom)

In addition, if you want to play with vty CLI version, the following dependencies
are also required:

* [text](http://hackage.haskell.org/package/text)
* [vty](http://hackage.haskell.org/package/vty)
* [vty-ui](http://hackage.haskell.org/package/vty-ui)

You can use following commands to run the program without cabal:

    cd src # assume your working directory is the project home.
    # to play the simple CLI version
    runhaskell MainSimple.hs
    # to play the vty CLI version
    runhaskell MainVty.hs

## How to play

keys:

* `q`: quit
* `i`: up
* `k`: down
* `j`: left
* `l`: right

If you are using `h2048-vty`, you can also use arrow keys.
