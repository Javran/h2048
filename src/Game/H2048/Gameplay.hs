module Game.H2048.Gameplay
  ( Gameplay
  , _gpRule
  , _gpScore
  , _gpBoard
  , _gpGen
  , mkGameplay
  , spawnNewCell
  , GameBoard
  , Cell
  , _cTier
  , Dir(..)
  , GameRule(..)
  , newGame
  , stepGame
  , standardGameRule
  , hasWon
  , isAlive
  , cellToInt
  , intToCell
  ) where

import Control.Monad.RWS.Strict
import System.Random.TF
import System.Random.TF.Instances

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Game.H2048.Core

{-
  Just a quick note that if we were to implement Gameplay
  in terms of monad transformer, MonadTransControl might be
  useful to allow passing IO actions to brick while keeping
  the stack of transformers.
 -}

data Gameplay
  = Gameplay
  { _gpRule :: GameRule -- game rules, must be read-only after creation.
  , _gpScore :: Int
  , _gpBoard :: GameBoard
  , _gpGen :: TFGen
  }

randomOp :: (TFGen -> (a, TFGen)) -> Gameplay -> (a, Gameplay)
randomOp op gp = (v, gp { _gpGen = g' })
  where
    g = _gpGen gp
    (v, g') = op g

mkGameplay :: TFGen -> GameRule -> Gameplay
mkGameplay g r =
  Gameplay
    r
    0
    M.empty -- default board is empty - no move is allowed on it.
    g

spawnNewCell :: Gameplay -> S.Set Coord -> Maybe (((Coord, Cell), S.Set Coord), Gameplay)
spawnNewCell gp emptyCells = do
  False <- pure $ S.null emptyCells
  let -- step 1: pick an empty cell.
      lowHigh = (0, S.size emptyCells - 1)
      (i, gp') = randomOp (randomR lowHigh) gp
      v = S.toAscList emptyCells !! i
      -- step 2: pick a tier.
      distrib = _grNewCellDistrib . _gpRule $ gp'
      (tier, gp'') = randomOp (randomPick distrib) gp'
  pure (((v, Cell tier), S.delete v emptyCells), gp'')

-- this function should only fail when there is insufficient space
-- to fill an empty board with # of initial cell spawn specified in GameRule.
newGame :: Gameplay -> Gameplay
newGame gp =
    fix (\loop curGp spawnTodo emptyCells ->
            if spawnTodo <= 0
              then curGp
              else case spawnNewCell curGp emptyCells of
                Nothing ->
                  error "Failed to create new game, no more space for empty cells."
                Just (((coord, cell), emptyCells'), curGp') ->
                  let curGp'' = curGp' { _gpBoard = M.insert coord cell (_gpBoard curGp') }
                  in loop curGp'' (spawnTodo - 1) emptyCells'
        )
      (gp { _gpScore = 0 })
      initSpawn
      (S.fromDistinctAscList coords)
  where
    rule = _gpRule gp
    coords = allCoords rule
    initSpawn = _grInitSpawn rule

{-
  Try to apply a move on current state of the game, returns:
  - `Nothing` if this move is invalid
  - `Just moves` if this move is valid, also returns all possible moves
    after the board is fully updated (meaning new cell has been spawned).
 -}
stepGame :: Dir -> Gameplay -> Maybe Gameplay
stepGame dir gp = do
  let rule = _gpRule gp
      coords = allCoords rule
      bd = _gpBoard gp
  {-
    if a move can be applied successfully, that means
    there must be empty cell on the board,
    in other words, spawnNewCell should not fail.
   -}
  (bd', award) <- applyMove rule dir bd
  let emptyCoords = S.filter (`M.notMember` bd') (S.fromDistinctAscList coords)
  (((coordNew, cellNew), _), gp') <- spawnNewCell gp emptyCoords
  pure gp'
    { _gpBoard = M.insert coordNew cellNew bd'
      {-
        This assumes that spawnNewCell does not change _gpScore,
        otherwise whatever update will be overwritten by following one.
       -}
    , _gpScore = _gpScore gp + award
    }

isAlive :: Gameplay -> Bool
isAlive gp = case possibleMoves (_gpRule gp) (_gpBoard gp) of
  [] -> False
  _ -> True

hasWon :: Gameplay -> Bool
hasWon gp = any (>= c2048) (_gpBoard gp)
  where
    c2048 = unsafeIntToCell 2048
