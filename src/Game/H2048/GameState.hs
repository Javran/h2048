module Game.H2048.GameState where

import Control.Monad.RWS.Strict
import Data.Function
import System.Random.TF
import System.Random.TF.Instances

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Game.H2048.NewCore

type GameplayT = RWST GameRule () GameState
type Gameplay = GameplayT IO

data GameState
  = GameState
    { _gsScore :: Int
    , _gsBoard :: GameBoard
    , _gsGen :: TFGen
    }

todo :: a
todo = error "TODO"

gameRandomR :: (Monad m, Random a) => (a, a) -> GameplayT m a
gameRandomR loHi = do
  g <- gets _gsGen
  let (v, g') = randomR loHi g
  v <$ modify (\s -> s { _gsGen = g'})

spawnNewCell :: Monad m => S.Set Coord -> GameplayT m (Maybe (Coord, S.Set Coord))
spawnNewCell emptyCells =
  if S.null emptyCells
    then pure Nothing
    else do
      i <- gameRandomR (0, S.size emptyCells - 1)
      let v = S.toAscList emptyCells !! i
      pure $ Just (v, S.delete v emptyCells)

newGame :: Monad m => GameplayT m ()
newGame = do
  (rowCnt, colCnt) <- asks _grDim
  initSpawn <- asks _grInitSpawn
  distrib <- asks (computeDistrib . _grNewCellDistrib)
  let allCells =
        S.fromList [ (r,c) | r <- [0..rowCnt-1], c <- [0..colCnt-1] ]
  newBoard <-
    fix (\loop curBoard needSpawn emptyCells ->
           if needSpawn == 0
             then pure curBoard
             else do
               m <- spawnNewCell emptyCells
               case m of
                 Just (coord, emptyCells') -> do
                   g <- gets _gsGen
                   let (tier, g') = randomPick distrib g
                   modify (\s -> s { _gsGen = g' })
                   let curBoard' = M.insert coord (Cell tier) curBoard
                   loop curBoard' (needSpawn - 1) emptyCells'
                 Nothing ->
                   error "Cannot create new game, insufficient space for empty cells."
        )
      M.empty
      initSpawn
      allCells
  modify (\s -> s { _gsScore = 0, _gsBoard = newBoard } )
