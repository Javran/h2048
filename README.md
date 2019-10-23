# h2048

[![Build Status](https://travis-ci.org/Javran/h2048.svg?branch=master)](https://travis-ci.org/Javran/h2048)

A haskell implementation of Game 2048. Including:

* a library for experimenting game strategies for Game 2048.
* a simple program that provides the basic interaction with the game.
* a CUI implemented using [brick](http://hackage.haskell.org/package/brick).

Based on [2048](https://github.com/gabrielecirulli/2048)

# Screenshots

## Simple version

![](https://raw.githubusercontent.com/Javran/h2048/master/doc/simple.jpg)

## brick-based version

![](https://raw.githubusercontent.com/Javran/h2048/master/doc/brick.jpg)

## Build and run

### With stack

h2048 comes with [stack](https://docs.haskellstack.org/en/stable/README/) configuration,
if you are building from source code, change directory to the project root and run:

```bash
stack build
# to run the brick version
stack exec -- h2048-brick
# to run the simple version
stack exec -- h2048-simple
```

### With cabal

`h2048` is now available on [hackage](http://hackage.haskell.org/package/h2048).

If you have [Cabal](http://www.haskell.org/cabal/) installed,
you can use the following command to install this project:

```bash
cabal update
cabal install h2048
```

The binaries are `h2048-simple` for simple CUI version, `h2048-brick` for CUI version
implemented using `brick`.

## How to play

keys:

* `q`: quit
* `i`: up
* `k`: down
* `j`: left
* `l`: right

If you are using `h2048-brick`, you can also use arrow keys.
