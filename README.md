# h2048


a haskell implementation of "2048"

Based on [2048](https://github.com/gabrielecirulli/2048)

## How to play

Run command `runhaskell h2048.hs` from your terminal.

keys:

* `q`: quit
* `i`: up
* `k`: down
* `j`: left
* `l`: right

## Dependencies

* [transformers](http://hackage.haskell.org/package/transformers-0.3.0.0)
* [MonadRandom](http://hackage.haskell.org/package/MonadRandom-0.1.3)

## TODO

* use CLI lib, potentially [hscurses](http://hackage.haskell.org/package/hscurses)
* test if the player wins, i.e.: a cell reaches 2048.

