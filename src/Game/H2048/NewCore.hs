module Game.H2048.NewCore where

{-
  TODO: this is an overhaul of Game.H2048.Core .. hmm just for fun.

  Differences will be:

  - Board is Map-based rather than any linear structure.
  - Use a newtype Cell = Cell Int to define tiers rather than using powers of 2.
  - Use tf-random for better random number generation.
  - Less verbosity on API: when initializing or after player has taken a move,
    the result will already have new cells inserted.
  - Also I want to explore how can strategy be factored out from core.
    For example, rules like the following is kind of arbitrary and one should
    be able to change them without having to modify core:

    - 90% time we get a 2, otherwise 4.
    - get a 2048 to be considered having won.

 -}
