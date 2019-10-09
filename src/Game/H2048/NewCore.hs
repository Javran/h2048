{-# LANGUAGE MonadComprehensions, TupleSections, LambdaCase #-}
module Game.H2048.NewCore where

import Control.Monad.ST
import Control.Monad.State
import Data.Bifunctor
import Data.Maybe
import Data.Monoid
import Data.Ord
import System.Random.TF
import System.Random.TF.Instances

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Search as VA

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
type GameBoard = M.Map Coord Cell

data GameState
  = GameState
    { _gsScore :: Int
    , _gsBoard :: GameBoard
    }

{-
  TODO: parametrize mergeAward and newCellDistrib on GameState.
  For now this is complicated and unnecessary.
 -}
data GameRule
  = GameRule
  { -- dimension of the board. (<# of rows>, <# of cols>)
    _grDim :: (Int, Int)
    -- whether the play has won given the current game state.
  , _grHasWon :: GameState -> Bool
    -- score awarded given GameState and Cell **before** the merge has happened.
  , _grMergeAward :: CellTier -> Int
    -- newly generated cell should follow this distribution from cell tier to a weight.
    -- note that values in this IntMap must be non-empty.
  , _grNewCellDistrib :: IM.IntMap Int
  }

mergeWithScore :: GameRule -> Cell -> Cell -> Maybe (Cell, Int)
mergeWithScore gr a b = do
  let Cell ctPrev = a
  c <- merge a b
  pure (c, _grMergeAward gr ctPrev)

mergeLine :: GameRule -> [Cell] -> ([Cell], Int)
mergeLine gr = mergeLine' 0
  where
    mergeLine' acc xs = case xs of
      a:b:ys
        | Just (c, award) <- mergeWithScore gr a b ->
            first  (c:) $ mergeLine' (acc+award) ys
      a:ys -> first (a:) (mergeLine' acc ys)
      [] -> ([], acc)

-- | move direction
data Dir
  = DUp
  | DDown
  | DLeft
  | DRight
  deriving (Enum, Bounded, Eq, Ord, Show)

type Coords = [Coord]
type CoordsGroup = [Coords]

dirToCoordsGroups :: GameRule -> Dir -> CoordsGroup
dirToCoordsGroups gr = \case
    DUp -> do
      c <- [0..cols-1]
      pure $ (,c) <$> [0..rows-1]
    DDown -> do
      c <- [0..cols-1]
      pure $ (,c) <$> [rows-1,rows-2..0]
    DLeft -> do
      r <- [0..rows-1]
      pure $ (r,) <$> [0..cols-1]
    DRight -> do
      r <- [0..rows-1]
      pure $ (r,) <$> [cols-1,cols-2..0]
  where
    (rows, cols) = _grDim gr

dirToCoords :: GameRule -> Int -> Dir -> [Coord]
dirToCoords gr rowOrCol dir = dirToCoordsGroups gr dir !! rowOrCol

-- extract a line (row or col) of cells from board
-- using the game move specified.
extractByCoords :: GameBoard -> [Coord] -> [Cell]
extractByCoords bd = mapMaybe (bd M.!?)

updateCoordsOnBoard :: [Coord] -> [Cell] -> GameBoard -> GameBoard
updateCoordsOnBoard coords vals =
    appEndo (foldMap (Endo . updateBoard) (zip coords mVals))
  where
    {-
      Note the use of "M.update" here - we need to do insertion and deletion
      at the same time and M.update does just that.

      Also coords should all be distinct, so it does not matter
      the order that this sequence of updates are performed.
     -}
    updateBoard (coord, mVal) = M.update (const mVal) coord
    mVals = (Just <$> vals) <> repeat Nothing

applyMoveOnCoords :: GameRule -> Coords -> GameBoard -> (GameBoard, Int)
applyMoveOnCoords gr coords bd =
    (updateCoordsOnBoard coords cells' bd, score)
  where
    cells = extractByCoords bd coords
    (cells', score) = mergeLine gr cells

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

randomPick :: V.Vector (a, Int) -> TFGen -> (a, TFGen)
randomPick vec g = runST $ do
    let upper = snd (V.last vec)
        (val, g') = randomR (1, upper) g
    -- safe because binary search is read-only.
    mv <- V.unsafeThaw vec
    {-
      Say if the accumulated distribution is like:

      > [1,3,5]

      Given 3, valid insertion points are:

      > [1,3,5]
           ^ ^
      But in our case we really want the lowest one, therefore using binarySearchLBy.
     -}
    ind <- VA.binarySearchLBy (comparing snd) mv (error "unused", val)
    pure (fst (vec V.! ind), g')

experiment :: Int -> [(Int, Int)] -> IO ()
experiment count xs = do
  let d = computeDistrib (IM.fromList xs)
  g <- newTFGen
  let picks =
        IM.fromListWith (+)
        . fmap (,1 :: Int)
        . evalState (replicateM count (state (randomPick d)))
        $ g
  mapM_ print (IM.toAscList picks)
