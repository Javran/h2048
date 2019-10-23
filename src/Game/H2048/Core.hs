{-# LANGUAGE
    MonadComprehensions
  , TupleSections
  , LambdaCase
  #-}
module Game.H2048.Core
  ( Coord
  , Dir(..)
  , Cell(..)
  , GameRule(..)
  , GameBoard
  , randomPick
  , allCoords
  , applyMove
  , possibleMoves
  , unsafeIntToCell
  , intToCell
  , cellToInt
  , standardGameRule
  , merge
  , mergeWithScore
  , mergeLine
  , dirToCoordsGroups
  , computeDistrib
  , testDistrib
  , isAlive
  ) where

import Control.Monad.ST
import Control.Monad.State
import Data.Bifunctor
import Data.Bits
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Tuple
import System.Random.TF
import System.Random.TF.Instances

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Search as VA

{-

  Note that this module should be considered internal, and only imported
  by Game.H2048.Gameplay or unit test modules.

  This is an overhaul and improvement of the old Game.H2048.Core module.

  Differences are:

  - Board is Map-based rather than any linear structure.
    This makes it convenient to change values or scale to support non-standard grid
    (i.e. any board other than 4x4)

  - Use a newtype Cell = Cell Int to define tiers rather than using powers of 2.
    This results in a compact representation of the board, and this also allows
    separating how data are displayed and how data are represented - you can
    use powers of two or some English words, which has no effect on the core itself.

  - Use tf-random for better random number generation.

  - Less verbosity on API. This module is now an internal one that only implements
    some key operations and all of the rest are implemented or re-exported through
    Game.H2048.Gameplay. This minimized the code for game logic on client side.

  - A GameRule data type for customizing game rules.

 -}

type CellTier = Int

newtype Cell = Cell { _cTier :: CellTier } deriving (Eq, Ord, Show)

{-
  Given that standard game is based on powers of 2, it makes sense
  that we export some direct support for it.
 -}
unsafeIntToCell :: Int -> Cell
unsafeIntToCell = Cell . countTrailingZeros

intToCell :: Int -> Maybe Cell
intToCell v = [ unsafeIntToCell v | v > 0, popCount v == 1 ]

cellToInt :: Cell -> Int
cellToInt (Cell t) = shiftL 1 t

merge :: Cell -> Cell -> Maybe Cell
merge (Cell a) (Cell b) = [ Cell (succ a) | a == b ]

type Coord = (Int, Int) -- (<row>, <col>) 0-based.

{-
  Note that the board could be empty to indicate that
  it has not been initialized yet.
 -}
type GameBoard = M.Map Coord Cell

{-
  TODO: parametrize mergeAward and newCellDistrib on GameState.
  For now this is complicated and unnecessary.
 -}
data GameRule
  = GameRule
  { -- dimension of the board. (<# of rows>, <# of cols>)
    _grDim :: (Int, Int)
    -- score awarded given GameState and Cell **before** the merge has happened.
  , _grMergeAward :: CellTier -> Int
    -- newly generated cell should follow this distribution from cell tier to a weight.
    -- note that values in this IntMap must be non-empty.
    -- use "computeDistrib" to compute this field.
  , _grNewCellDistrib :: V.Vector (Int, Int)
    -- how many cells to spawn at the beginning of a game.
  , _grInitSpawn :: Int
  , _grHasWon :: GameBoard -> Bool
  }

standardGameRule :: GameRule
standardGameRule = GameRule
    { _grDim = (4,4)
    , _grInitSpawn = 2
    , _grNewCellDistrib = computeDistrib $ IM.fromList [(1, 9), (2, 1)]
    , _grHasWon =
        let c2048 = unsafeIntToCell 2048
        in any (>= c2048)
    , _grMergeAward = \prevTier -> shiftL 1 (prevTier+1) -- 2^(prevTier+1)
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

-- extract a line (row or col) of cells from board
-- using the game move specified.
extractByCoords :: GameBoard -> [Coord] -> [Cell]
extractByCoords bd = mapMaybe (bd M.!?)

-- return a unique, sorted list of all coordinations of a board.
allCoords :: GameRule -> Coords
allCoords GameRule { _grDim = (rowCnt, colCnt) } =
  [ (r,c) | r <- [0..rowCnt-1], c <- [0..colCnt-1] ]

alterCoordsOnBoard :: [Coord] -> [Cell] -> GameBoard -> GameBoard
alterCoordsOnBoard coords vals =
    appEndo (foldMap (Endo . alterBoard) (zip coords mVals))
  where
    {-
      Note the use of "M.alter" here - we need to do insertion and deletion
      at the same time and M.alter does just that.

      Also note that "M.update" cannot be used here because it does not insert if missing.

      Also coords should all be distinct, so it does not matter
      the order that this sequence of updates are performed.
     -}
    alterBoard (coord, mVal) = M.alter (const mVal) coord
    mVals = (Just <$> vals) <> repeat Nothing

applyMoveOnCoords :: GameRule -> Coords -> GameBoard -> (GameBoard, Int)
applyMoveOnCoords gr coords bd =
    (alterCoordsOnBoard coords cells' bd, score)
  where
    cells = extractByCoords bd coords
    (cells', score) = mergeLine gr cells

-- apply a game move on a board.
applyMove :: GameRule -> Dir -> GameBoard -> Maybe (GameBoard, Int)
applyMove gr dir bd =
    {-
      Note that a GameBoard could be empty to indicate that it is uninitialized,
      in which case every move will fail because no change on the board could be made.
     -}
    [ (bd', score) | bd /= bd' ]
  where
    csGroups = dirToCoordsGroups gr dir
    (scores, bd') =
      runState
        (mapM (\coords -> state (swap . applyMoveOnCoords gr coords)) csGroups)
        bd
    score = sum (scores :: [Int])

possibleMoves :: GameRule -> GameBoard -> [Dir]
possibleMoves gr bd =
  [ d | d <- [minBound .. maxBound], isJust (applyMove gr d bd) ]

{-
  Pre-processing the distribution:

  e.g. {a: 3, b: 4, c: 2}
  => [(a, 3), (b, 3+4), (c, 3+4+2)] = [(a, 3), (b, 7), (c, 9)]

  after this is done, we can pick a value
  from 1 to the last element of this vector (in this case, 9.),
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

testDistrib :: Int -> [(Int, Int)] -> IO ()
testDistrib count xs = do
  let d = computeDistrib (IM.fromList xs)
  g <- newTFGen
  let picks =
        IM.fromListWith (+)
        . fmap (,1 :: Int)
        . evalState (replicateM count (state (randomPick d)))
        $ g
  mapM_ print (IM.toAscList picks)

isAlive :: GameRule -> GameBoard -> Bool
isAlive gr bd = not . null $ possibleMoves gr bd
