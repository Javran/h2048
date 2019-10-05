{-# LANGUAGE MonadComprehensions #-}
module Game.H2048.NewCore where

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

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

    - size of the board is 4x4. (_grDim)
    - 90% time we get a 2, otherwise 4. (_grNewCellDistrib)
    - get a 2048 to be considered having won. (_grHasWon)
    - how many score you get by merging. (_grMergeAward)

 -}

type CellTier = Int

newtype Cell = Cell { _cTier :: CellTier }

merge :: Cell -> Cell -> Maybe Cell
merge (Cell a) (Cell b) = [ Cell (succ a) | a == b ]

type Coord = (Int, Int) -- (<row>, <col>) 0-based.

data GameState
  = GameState
    { _gsScore :: Int
    , _gsBoard :: M.Map Coord Cell
    }

data GameRule
  = GameRule
  { -- dimension of the board. (<# of rows>, <# of cols>)
    _grDim :: (Int, Int)
    -- whether the play has won given the current game state.
  , _grHasWon :: GameState -> Bool
    -- score awarded given GameState and Cell **before** the merge has happened.
  , _grMergeAward :: GameState -> CellTier -> Int
    -- newly generated cell should follow this distribution from cell tier to a weight.
    -- note that values in this IntMap must be non-empty.
  , _grNewCellDistrib :: GameState -> IM.IntMap Int
  }

{-
  Pre-processing the distribution:

  e.g. {a: 3, b: 4, c: 2} => [(a, 3), (b, 3+4), (c, 3+4+2)] = [(a, 3), (b, 7), (c, 9)]

  after this is done, we can pick a value from 1 to the last element of this vector (in this case, 9.),
  and lookup the corresponding element.

 -}
computeDistrib :: IM.IntMap Int -> V.Vector (Int, Int)
computeDistrib m =
    V.fromListN (IM.size m) $ zip (fmap fst pairs) weights
  where
    pairs = IM.toList m
    weights = scanl1 (+) . fmap snd $ pairs
