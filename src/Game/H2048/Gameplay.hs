module Game.H2048.Gameplay where

import Control.Monad.RWS.Strict
import System.Random.TF
import System.Random.TF.Instances

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Game.H2048.NewCore

{-
  Just a quick note that if we were to implement Gameplay
  in terms of monad transformer, MonadTransControl might be
  useful to allow passing IO actions to brick while keeping
  the stack of transformers.
 -}

data GameState
  = GameState
    { _gsScore :: Int
    , _gsBoard :: GameBoard
    , _gsGen :: TFGen
    }

data Gameplay
  = Gameplay
  { _gpRule :: GameRule -- game rules, must be read-only after creation.
  , _gpScore :: Int
  , _gpBoard :: GameBoard
  , _gpGen :: TFGen
  }

todo :: a
todo = error "TODO"

randomOp :: (TFGen -> (a, TFGen)) -> Gameplay -> (a, Gameplay)
randomOp op gp = (v, gp { _gpGen = g' })
  where
    g = _gpGen gp
    (v, g') = op g

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
