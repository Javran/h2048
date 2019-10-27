{-# LANGUAGE
    MonadComprehensions
  , TupleSections
  , LambdaCase
  #-}
{-|
  This module is considered internal.
  Clients should use "Game.H2048.Gameplay" instead.
 -}
module Game.H2048.Core
  ( Coord
  , Dir(..)
  , CellTier
  , Cell(..)
  , Distrib
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

{-|
  A `CellTier` is simply a positive `Int`. Every time two cell merges,
  the tier of the resulting cell increases by one relative to cell tier
  prior to the merge.
 -}
type CellTier = Int

{-|
  An obscure data type that wraps 'CellTier'.
  -}
newtype Cell =
  Cell
  { _cTier :: CellTier -- ^ Tier of this cell.
  } deriving (Eq, Ord, Show)

{-|
  Convert an integer to 'Cell', the input is expected
  to be a power of 2 but no check is enforced.

  Given that standard game is based on powers of 2, it makes sense
  that we implement some direct support for it.
 -}
unsafeIntToCell :: Int -> Cell
unsafeIntToCell = Cell . countTrailingZeros

{-|
  Safely convert a power of two into 'Cell'.
 -}
intToCell :: Int -> Maybe Cell
intToCell v = [ unsafeIntToCell v | v > 0, popCount v == 1 ]

{-|
  Convert 'Cell' back into a power of 2.
 -}
cellToInt :: Cell -> Int
cellToInt (Cell t) = shiftL 1 t

merge :: Cell -> Cell -> Maybe Cell
merge (Cell a) (Cell b) = [ Cell (succ a) | a == b ]

{-|
  Zero-based @(rowIndex, colIndex)@.
 -}
type Coord = (Int, Int)

{-|
  A 'GameBoard' is a map from coordinates to 'Cell's for a game.

  Note that the map could be empty to indicate that
  a new game is not started yet.
 -}
type GameBoard = M.Map Coord Cell

type Distrib' a = V.Vector (a, Int)
{-|
  A 'Distrib' is a non-empty 'V.Vector' whose each element @(a,b)@ satisfies:

  * @a@, when taken in sequence, is positive and strictly increasing.
  * @b@, when taken in sequence, is strictly increasing.

  Think this data type as a precomputation result for
  making weighted random choice.

  You can use 'computeDistrib' to generate a value of this.
 -}
type Distrib = Distrib' Int

{-|
  A data type for encoding game rules that do not necessarily
  needs to be hard-coded into core logic.

  You can use 'standardGameRule' for a standard game rule,
  or make changes using it as the base.
 -}
data GameRule
  = GameRule
  { {-|
      Dimension of the board. @(numOfRows, numOfCols)@
      -}
    _grDim :: (Int, Int)
    {-|
       Score awarded given 'CellTier' /before/ the merge has happened.
     -}
  , _grMergeAward :: CellTier -> Int
    {-|
      Stores precomputation result that encodes distribution of tiers
      of newly spawned cells.
     -}
  , _grNewCellDistrib :: Distrib
    {-|
      How many initial cells should be spawned when starting the game.

      Note this value should not exceed number of cells that the board can contain.
     -}
  , _grInitSpawn :: Int
    {-|
      A predicate to tell whether the current game has been won.

      The standard game rule is an interesting one: no more valid moves
      does not imply losing the game. Instead, getting any cell to 2048
      (or to tier 11 using this core's terminology) is considerred winning.

      The intention is to abstract this part out to have fun experimenting.
     -}
  , _grHasWon :: Int -> GameBoard -> Bool
  }

{-|
  The standard game rule. This value can be used as a base
  for customized game rules.
 -}
standardGameRule :: GameRule
standardGameRule = GameRule
    { _grDim = (4,4)
    , _grInitSpawn = 2
    , _grNewCellDistrib = computeDistrib $ IM.fromList [(1, 9), (2, 1)]
    , _grHasWon = \_score ->
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

-- | Moves that a user could do.
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
{-|
  Computes `Distrib` for weighted random cell tier spawns.

  The input must be a non-empty map from cell tiers to their
  corresponding weight. All weights must be positive.
 -}
computeDistrib :: IM.IntMap Int -> Distrib
computeDistrib m =
    V.fromListN (IM.size m) $ zip (fmap fst pairs) weights
  where
    pairs = IM.toList m
    weights = scanl1 (+) . fmap snd $ pairs

randomPick :: Distrib' a -> TFGen -> (a, TFGen)
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
