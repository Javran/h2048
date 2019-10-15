module Game.H2048.GameState where

import Game.H2048.NewCore

data GameState
  = GameState
    { _gsScore :: Int
    , _gsBoard :: GameBoard
    }
