module Game.H2048.Gameplay where

import Control.Monad.RWS.Strict
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

spawnNewCell :: Monad m => S.Set Coord -> GameplayT m (Maybe ((Coord, Cell), S.Set Coord))
spawnNewCell emptyCells =
  if S.null emptyCells
    then pure Nothing
    else do
      i <- gameRandomR (0, S.size emptyCells - 1)
      distrib <- asks _grNewCellDistrib
      let v = S.toAscList emptyCells !! i
      g <- gets _gsGen
      let (tier, g') = randomPick distrib g
      modify (\s -> s { _gsGen = g' })
      pure $ Just ((v, Cell tier), S.delete v emptyCells)

-- this function should only fail when there is insufficient space
-- to fill an empty board with # of initial cell spawn specified in GameRule.
newGame :: Monad m => GameplayT m ()
newGame = do
  (rowCnt, colCnt) <- asks _grDim
  initSpawn <- asks _grInitSpawn
  let allCells =
        S.fromList [ (r,c) | r <- [0..rowCnt-1], c <- [0..colCnt-1] ]
  newBoard <-
    fix (\loop curBoard needSpawn emptyCells ->
           if needSpawn == 0
             then pure curBoard
             else do
               m <- spawnNewCell emptyCells
               case m of
                 Just ((coord, cell), emptyCells') ->
                   let curBoard' = M.insert coord cell curBoard
                   in loop curBoard' (needSpawn - 1) emptyCells'
                 Nothing ->
                   error "Failed to create new game, no more space for empty cells."
        )
      M.empty
      initSpawn
      allCells
  modify (\s -> s { _gsScore = 0, _gsBoard = newBoard } )

{-

  Try to apply a move on current state of the game, returns:

  - `Nothing` if this move is invalid
  - `Just moves` if this move is valid, also returns all possible moves
    after the board is fully updated (meaning new cell has been spawned).
 -}
stepGame :: Monad m => Dir -> GameplayT m (Maybe [Dir])
stepGame dir = do
  gr <- ask
  (rowCnt, colCnt) <- asks _grDim
  bd <- gets _gsBoard
  case applyMove gr dir bd of
    Nothing -> pure Nothing
    Just (bd', award) -> do
      modify (\s -> s { _gsScore = _gsScore s + award })
      -- if a move can be applied successfully, that means
      -- there must be empty cell on the board,
      -- in other words, spawnNewCell should not fail.
      let allCoords =
            S.fromList [ (r,c) | r <- [0..rowCnt-1], c <- [0..colCnt-1] ]
          emptyCoords = S.filter (`M.notMember` bd') allCoords
      mResult <- spawnNewCell emptyCoords
      case mResult of
        Nothing -> error "Failed to spawn new cell, this should not happen."
        Just ((coord, cell), _) -> do
          let bd'' = M.insert coord cell bd'
          modify (\s -> s { _gsBoard = bd'' })
          pure $ Just (possibleMoves gr bd'')
