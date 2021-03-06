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
  , Coords
  , CoordsGroup
  , Dir(..)
  , CellTier
  , Cell(..)
  , Distrib'
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

{-|
  Attempt to merge two 'Cell' s.

  Only successful when two `Cell`s are equal, resulting in a new Cell
  with tier increased by 1.
 -}
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

{-|
  The same as 'Distrib' except parameterized on the value type.
 -}
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
     -}
    {-
      Some extra notes for those that want to read a bit more:

      Notice that this core logic only cares about whether we have valid moves
      on a GameBoard (see also 'isAlive' below) but does not use '_grHasWon' at all.

      In fact, in the standard game rule, whether we are winning and whether we have
      valid moves are sort of independent of each other.
      This means we can minimize core logic by letting client-facing API module
      take responsibility for handling the winning logic.

      Despite not being used by core, we still keep it here, because:

      * it makes sense as GameRule is literally the set of things that dictates the gameplay.
      * modules that implements things on top of this core don't need to
        add another layer of data type to include extra stuff that they would need.
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

{-|
  Merge two cells with a reward as specified by 'GameRule'.
 -}
mergeWithScore :: GameRule -> Cell -> Cell -> Maybe (Cell, Int)
mergeWithScore gr a b = do
  let Cell ctPrev = a
  c <- merge a b
  pure (c, _grMergeAward gr ctPrev)

{-|
  Merge a single line of cells, return the resulting line and
  scores awarded according to the 'GameRule'.
 -}
{-
  Notice that input and output are both lists:

  * Moving on one direction squeezes out those empty cells,
    therefore it is not necessary to consider empty cells at all.
  * Similar rationale for output type - the result is always
    a line of cells free of empty ones in between any of those.
 -}
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

{-|
  List of 'Coord'. This list
  is usually a complete row or column in the game board.
 -}
type Coords = [Coord]

{-|
  List of 'Coords', expected to exact-cover the game board.
 -}
type CoordsGroup = [Coords]

{-|
  Given a game move,
  return rows or columns of 'Coords' that forms the complete board.
 -}
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

{-|
  Retrieve a list of cells from game board.
  This operation preserves order. Empty cells are excluded from the result.
 -}
extractByCoords :: GameBoard -> [Coord] -> [Cell]
extractByCoords bd = mapMaybe (bd M.!?)

{-|
  Return a unique, sorted list of all coordinations of a board.
 -}
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

{-|
  Apply a game move on certain part of the board specified by @Coords@.
 -}
applyMoveOnCoords :: GameRule -> Coords -> GameBoard -> (GameBoard, Int)
applyMoveOnCoords gr coords bd =
    (alterCoordsOnBoard coords cells' bd, score)
  where
    cells = extractByCoords bd coords
    (cells', score) = mergeLine gr cells

{-|
  Apply a game move on a board.
  This operation fails if and only if the move results in no change to the game board.
 -}
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

{-|
  Return possible moves that can be performed on current board.
 -}
possibleMoves :: GameRule -> GameBoard -> [(Dir, (GameBoard, Int))]
possibleMoves gr bd =
  mapMaybe (\d -> (d,) <$> applyMove gr d bd) [minBound .. maxBound]

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

{-|
  Pick a value randomly following the distribution as specified by the argument.
 -}
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

{-|
  Repeat the process of randomly picking elements following a distribution in @IO@.

  This function is exported just for manual testing.
 -}
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

{-|
  A current game is consider \"alive\" when there are at least one
  valid move for the current board.

  Note that since a GameBoard can be newly initiate as empty Map,
  it is not \"alive\" by definition.
 -}
isAlive :: GameRule -> GameBoard -> Bool
isAlive gr bd = not . null $ possibleMoves gr bd
