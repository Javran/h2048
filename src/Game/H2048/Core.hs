{-|
  Module      : Game.H2048.Core
  Copyright   : (c) 2014 Javran Cheng
  License     : MIT
  Maintainer  : Javran.C@gmail.com
  Stability   : experimental
  Portability : POSIX

The core game logic implementation for Game 2048.

The routine for using this library would be:

1. use `initGameBoard` to get a valid board to begin with.
(two new cells are inserted for you, if you want to use an empty board,
`initBoard` is a shorthand)

2. interact with user / algorithm / etc., use `updateBoard` to update a board.

3. use `insertNewCell` to insert a new cell randomly

4. examine if the player wins / loses / is still alive using `gameState`.

-}
{-# LANGUAGE TupleSections, FlexibleContexts #-}
module Game.H2048.Core
  ( Dir (..)
  , BoardUpdateResult
    -- TODO: shouldn't expose constructor.
  , Board
  , mkBoard
  , fromBoard
  , Line
  , mkLine
  , GameState(..)
  , gameState
  , compactLine
  , initGameBoard
  , updateBoard
  , insertNewCell
  , generateNewCell
  , nextMoves
  )
where

import Data.List
import Data.Maybe
import Data.Default

import Control.Monad.Writer
import Control.Monad.Random
import Data.Coerce
import Control.Lens

import Game.H2048.Utils

-- | represent a 4x4 board for Game 2048
--   each element should be either zero or 2^i
--   where i >= 1.
newtype Board = Board [Line]

mkBoard :: [[Int]] -> Board
mkBoard = Board . take 4 . (++ repeat def) . (mkLine <$>)

fromBoard :: Board -> [[Int]]
fromBoard = coerce

-- | a list of 4 elements, stands for
--   one column / row in the board
{-
  TODO:
  alternatively Line could be a ~ Int => (a,a,a,a)
  and Board being a ~ Line -> (a,a,a,a),
  we should be able to utilize fixed-vector.
-}
newtype Line = Line [Int] deriving (Eq, Show)

mkLine :: [Int] -> Line
mkLine = Line . take 4 . (++ repeat 0)

instance Default Line where
    def = mkLine []

-- | result after a successful 'updateBoard'
type BoardUpdateResult = (Board, Int)

-- | current game state, see also 'gameState'
data GameState
  = Win       -- ^ win, can make no step further
  | WinAlive  -- ^ win, and still alive
  | Lose      -- ^ can make no step further, no cell reaches 2048
  | Alive     -- ^ playing
  deriving (Enum, Eq, Show)
{-
  notice that we don't need GameState at all:
  - at any point in time, the game is won when we have a cell that contains a value
    >= 2048
  - when there are possible moves, we are not done yet,
    otherwise the game is lost.

  therefore we could probably return a list of pairs of
  (<move>, <board-after-that-move), so making the next move is
  as simple as picking a move and replace state with `snd` part of that move.
  and if the list is empty, we know there are no more moves and we are done.
-}

-- | move direction
data Dir
  = DUp
  | DDown
  | DLeft
  | DRight
  deriving (Enum, Bounded, Eq, Ord, Show)

allDirs :: [Dir]
allDirs = [minBound .. maxBound]

-- | the initial board before a game started
instance Default Board where
    def = mkBoard []

-- | move each non-zero element to their leftmost possible
--   position while preserving the order
compactLine :: MonadWriter (Sum Int) m => Line -> m Line
compactLine (Line l) = mkLine <$> merge (filter (/= 0) l)
    where
      merge r = case r of
        (x:y:xs) ->
          if x == y
            -- only place where score are collected.
            then
              -- try to merge first two elements,
              -- and process rest of it.
              let s = x+y
              in tell (Sum s) >> (s:) <$> merge xs
            else
              -- just skip the first one,
              -- and process rest of it.
              (x:) <$> merge (y:xs)
        _ -> pure r

-- | update the board taking a direction,
--   a 'BoardUpdated' is returned on success,
--   if this update does nothing, that means a failure (Nothing)
updateBoard :: Dir -> Board -> Maybe BoardUpdateResult
updateBoard d (Board board) = do
    guard $ board /= board'
    pure (Board board', getSum score)
  where
        -- transform boards so that
        -- we only focus on "gravitize to the left".
        -- and convert back after the gravitization is done.
        (board',score) = runWriter $ withIso (c . rTrans . from c) $
          \g f -> g <$> mapM compactLine (f board)

        c :: Iso' [Line] [[Int]]
        c = coerced
        rTrans :: Iso' [[Int]] [[Int]]
        rTrans =
            case d of
              DLeft -> id
              DRight -> sRight
              DUp -> sUp
              DDown -> sUp . sRight
          where
            sRight = involuted (map reverse)
            sUp = involuted transpose

nextMoves :: Board -> [(Dir, BoardUpdateResult)]
nextMoves b = mapMaybe (\d -> (d,) <$> updateBoard d b) allDirs

-- | find blank cells in a board,
--   return coordinates for each blank cell
blankCells :: Board -> [(Int, Int)]
blankCells (Board b) = map (\(row, (col, _)) -> (row,col)) blankCells'
  where
    {-
      the algorithm is to just find all empty cells -
      we could of course keep track of all empty cells,
      but that will be overcomplicated and hard to maintain
      when we do "compactLine"
     -}
    blankCells' = filter ((== 0) . snd . snd) linearBoard
    -- flatten to make it ready for filter
    linearBoard = concat $ zipWith tagRow [0..] colTagged

    -- tag cells with row num
    tagRow row = map ((,) row)
    -- tag cells with column num
    colTagged = map (zip [0..] . (coerce :: Line -> [Int])) b

-- | return current game state.
--   'Win' if any cell is equal to or greater than 2048
--   or 'Lose' if we can move no further
--   otherwise, 'Alive'.
gameState :: Board -> GameState
gameState nb@(Board b)
    | isWin
        = if noFurther
              then Win
              else WinAlive
    | noFurther
        = Lose
    | otherwise
        = Alive
    where
        isWin = (any (>= 2048) . concatMap (coerce :: Line -> [Int])) b
        noFurther = null (nextMoves nb)

-- | initialize the board by puting two cells randomly
--   into the board.
--   See 'generateNewCell' for the cell generating rule.
initGameBoard :: (MonadRandom r) => r (Board, Int)
initGameBoard =
    -- insert two cells and return the resulting board
    -- here we can safely assume that the board has at least two empty cells
    -- so that we can never have Nothing on the LHS
    (,0) . fromJust <$> (insertNewCell def >>= (insertNewCell . fromJust))

-- | try to insert a new cell randomly
insertNewCell :: (MonadRandom r) => Board -> r (Maybe Board)
insertNewCell b = do
    -- get a list of coordinates of blank cells
    let availableCells = blankCells b

    if null availableCells
       -- cannot find any empty cell, then fail
       then return Nothing
       else do
           -- randomly pick up an available cell by choosing index
           choice <- getRandomR (0, length availableCells - 1)
           let (row,col) = availableCells !! choice
           value <- generateNewCell
           let (Board b') = b
               c1 :: ([Int] -> [Int]) -> Line -> Line
               c1 = coerce
           pure $ Just $ Board $ (inPos row . c1 . inPos col) (const value) b'

-- | generate a new cell according to the game rule
--   we have 90% probability of getting a cell of value 2,
--   and 10% probability of getting a cell of value 4.
generateNewCell :: (MonadRandom r) => r Int
generateNewCell = getRandom >>= \r ->
    pure $ if r < (0.9 :: Float) then 2 else 4
