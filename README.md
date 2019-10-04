# h2048

[![Build Status](https://travis-ci.org/Javran/h2048.svg?branch=master)](https://travis-ci.org/Javran/h2048)

A haskell implementation of Game 2048. Including:

* a library for experimenting game strategies for Game 2048
* a simple CLI that merely pretty-prints the game board
* a better CLI implemented using [brick](http://hackage.haskell.org/package/brick)

Based on [2048](https://github.com/gabrielecirulli/2048)

# Screenshots

## Simple CLI version

![](http://i.imgur.com/FS3tdLp.jpg)

## vty CLI version

![](http://i.imgur.com/twNDMeq.jpg)

## Build and run

### With cabal

`h2048` is now available on [hackage](http://hackage.haskell.org/package/h2048).

If you have [Cabal](http://www.haskell.org/cabal/) installed,
you can use the following command to install this project:

    cabal update
    cabal install h2048

The binaries are `h2048-simple` for simple CLI version, `h2048-brick` for CLI version
implemented using `brick`.

### Without cabal

First make sure the following dependencies are installed:

* [mtl](http://hackage.haskell.org/package/mtl)
* [MonadRandom](http://hackage.haskell.org/package/MonadRandom)

In addition, if you want to play with vty CLI version, the following dependencies
are also required:

* [text](http://hackage.haskell.org/package/text)
* [vty](http://hackage.haskell.org/package/vty)
* [brick](http://hackage.haskell.org/package/brick)

## How to play

keys:

* `q`: quit
* `i`: up
* `k`: down
* `j`: left
* `l`: right

If you are using `h2048-brick`, you can also use arrow keys.
