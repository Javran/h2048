module Game.H2048.Gameplay where

import Control.Monad.RWS.Strict
import System.Random.TF
import System.Random.TF.Instances

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Game.H2048.NewCore

data GameState
  = GameState
    { _gsScore :: Int
    , _gsBoard :: GameBoard
    , _gsGen :: TFGen
    }

data Gameplay
  = Gameplay
  { _gpRule :: GameRule -- game rules, must be read-only after creation.
  , _gpState :: GameState -- the only mutating bit of gameplay.
  }

todo :: a
todo = error "TODO"

