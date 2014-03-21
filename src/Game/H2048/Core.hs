module Game.H2048.Core
    ( Board       -- make sure board cannot be crafted from outside
    , Line
    , Dir (..)
    , BoardResult (..)
    , isDead
    , compactLine
    , initBoard
    , initGame
    , updateBoard
    , insertNewCell
    , generateNewCell
    )
where

import Control.Arrow
import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Maybe

import Game.H2048.Utils

-- | represent a 4x4 board for Game 2048
--   each element should be either zero or 2^i
--   where 1 <= i <= 11.
type Board = [[Int]]

-- | a list of 4 elements, stands for
--   one column / row in the board
type Line  =  [Int]

-- | result after a successful 'updateBoard'
data BoardResult = BoardResult
    { brBoard    :: Board  -- ^ new board
    , brScore    :: Int    -- ^ score collected in this update
    } deriving (Eq, Show)

-- | the move direction
data Dir = DUp
         | DDown
         | DLeft
         | DRight
         deriving (Enum, Bounded, Eq, Ord, Show)

-- | the initial board before a game started
initBoard :: Board
initBoard = replicate 4 $ replicate 4 0

-- | move each non-zero element to their leftmost possible
--   position while preserving the order
compactLine :: Line -> (Line, Int)
              -- remove zeros
compactLine =     filter (/=0)
              -- do merge and collect score
              >>> merge
              -- restore zeros, on the "fst" part
              >>> first (take 4 . (++ repeat 0))
    where
        merge :: [Int] -> ([Int], Int)
        merge (x:y:xs) =
            if x == y
                -- only place where score are collected.
                then let (xs', score') = merge xs
                     -- try to merge first two elements,
                     -- and process rest of it.
                     in ((x+y) : xs', x+y+score')
                else let (xs', score') = merge (y:xs)
                     -- just skip the first one,
                     -- and process rest of it.
                     in (x:xs', score')
        merge r = (r, 0)

-- | update the board taking a direction,
--   a "BoardResult" is returned on success,
--   if this update does nothing, that means a failure (Nothing)
updateBoard :: Dir -> Board -> Maybe BoardResult
-- TODO: use writer monad?
updateBoard d board = if board /= board'
                          then Just $ BoardResult board' score
                          else Nothing
    where
        board' :: Board
        -- transform boards so that
        -- we only focus on "gravitize to the left".
        -- and convert back after the gravitization is done.
        (board',score) =     rTransL         -- transform to a "gravitize to the left" problem
                         >>> map compactLine -- gravitize to the left
                         >>> unzip           -- split result into Board and score collected
                         >>> first rTransR   -- transform back
                         >>> second sum      -- get all scores earned in this turn
                           $ board
        -- rTrans for "a list of reversible transformations, that will be performed in order"
        rTrans :: [Board -> Board]
        rTrans =
            case d of
              -- the problem itself is "gravitize to the left"
              DLeft  -> []
              -- we use a mirror
              DRight -> [map reverse]
              -- diagonal mirror
              DUp    -> [transpose]
              -- same as DUp case + DRight case
              DDown  -> [transpose, map reverse]

        -- how we convert it "into" and "back"
        rTransL = foldl (flip (.)) id rTrans
        rTransR = foldr       (.)  id rTrans

-- | find blank cells in a board,
--   return coordinates for each blank cell
blankCells :: Board -> [(Int, Int)]
blankCells b = map (\(row, (col, _)) -> (row,col)) blankCells'
    where
        blankCells' = filter ((== 0) . snd . snd) linearBoard
        -- flatten to make it ready for filter
        linearBoard = concat $ zipWith tagRow [0..] colTagged

        -- tag cells with row num
        tagRow row = map ( (,) row )
        -- tag cells with column num
        colTagged = map (zip [0..]) b

-- | the board is dead if there is no valid move
isDead :: Board -> Bool
isDead b = all (\d -> isNothing $ updateBoard d b) universe

-- | initialize the board by puting two cells randomly
--   into the board.
--   See "generateNewCell" for the cell generating rule.
initGame :: (MonadRandom r) => r (Board, Int)
initGame =
    -- insert two cells and return the resulting board
    -- here we can safely assume that the board has at least two empty cells
    -- so that we can never have Nothing on the LHS
    liftM ( (\x -> (x,0)) . fromJust) (insertNewCell initBoard >>= (insertNewCell . fromJust))

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
           return $ Just $ (inPos row . inPos col) (const value) b

-- | generate a new cell according to the game rule
--   we have 90% probability of getting a cell of value 2,
--   and 10% probability of getting a cell of value 4.
generateNewCell :: (MonadRandom r) => r Int
generateNewCell = do
    r <- getRandom
    return $ if r < (0.9 :: Float) then 2 else 4
