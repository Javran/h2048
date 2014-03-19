module Game.H2048.Core
-- TODO: minimize export
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.List
import Data.Maybe
import System.IO
import Text.Printf

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
    , brNextDirs :: [Dir]  -- ^ all possible next moves
    } deriving (Eq, Show)

-- | the move direction
data Dir = DUp
         | DDown
         | DLeft
         | DRight
         deriving (Enum, Bounded, Eq, Ord, Show)

-- | all possible values for a Bounded Enum
universe :: (Bounded e, Enum e) => [e]
universe = [minBound .. maxBound]

-- | the initial board before a game started
initBoard :: Board
initBoard = replicate 4 $ replicate 4 0

-- | pretty print the board to stdout
drawBoard :: Board -> IO ()
drawBoard board = do
    {-
     when outputed, a cell will look like:

       +-----+
       | xxx |
       +-----+

     the pretty-printing strategy is to print the first line
     and for each row in the board:

     * print the leftmost "| "
     * let each cell in the row print " <number> |"
     * finalize this line by printing out the horizontal "+--+--+..."
    -}
    putStrLn horizSeparator
    mapM_ drawRow board
    where
        cellWidth = length " 2048 "
        -- build up the separator: "+--+--+....+"
        horizSeparator' =
            intercalate "+" (replicate 4 (replicate cellWidth '-'))
        horizSeparator = "+" ++ horizSeparator' ++ "+"

        drawRow :: [Int] -> IO ()
        drawRow row = do
            -- prints "| <cell1> | <cell2> | ... |"
            putChar '|'
            mapM_ (printf " %4d |") row
            putChar '\n'
            putStrLn horizSeparator

-- | move each non-zero element to their leftmost possible
--   position while preserving the order
compactLine :: Line -> Line
compactLine = bracketF
                  (filter (/=0))           -- remove zeros before processing
                  (take 4 . (++ repeat 0)) -- bring back zeros after processomg
                  (fst . merge)            -- try to merge
    where
        merge :: [Int] -> ([Int], Int)
        merge (x:y:xs) =
            if x == y
                -- only place where score are collected.
                then let (xs', score') = merge xs
                     in ((x+y) : xs', x+y+score')
                else let (xs', score') = merge (y:xs)
                     in (x:xs', score')
        merge r = (r, 0)

-- | when player moves, give the next board before adding random cells into it
--   returns the board after modification together with a boolean value.
--   the boolean value is False only if this movement changes nothing.
updateBoard :: Dir -> Board -> (Board, Bool)
-- TOOD: - snd of the return value not used for now
--       - should collect score
updateBoard d board = (board', board /= board')
    where
        board' :: Board
        -- transform boards so that
        -- we only focus on "gravitize to the left".
        -- and convert back after the gravitization is done.
        board' = bracketF rTransL rTransR (map compactLine) board
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

{-# ANN bracketF "HLint: ignore Redundant bracket" #-}
-- | use first argument to convert data,
--   third argument to perform operations,
--   second argument to convert back
bracketF :: (c -> a) -- ^ open bracket
         -> (b -> d) -- ^ close bracket
         -> (a -> b) -- ^ what's in the bracket
         -> (c -> d) -- ^ the resulting function
bracketF openF closeF f = closeF . f . openF

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

-- | play game on a given board until user quits or game ends
playGame :: (RandomGen g) => Board -> RandT g IO ()
playGame b = do
    (quit,newB) <- liftIO $ do
        drawBoard b
        hFlush stdout

        c <- getChar

        putStrLn ""
        hFlush stdout

        -- TODO: customizable
        key <- case c of
          'q' -> return Nothing
          'i' -> putStrLn "Up" >> return (Just DUp)
          'k' -> putStrLn "Down" >> return (Just DDown)
          'j' -> putStrLn "Left" >> return (Just DLeft)
          'l' -> putStrLn "Right" >> return (Just DRight)
          _ -> putStrLn "'i'/'k'/'j'/'l' to move, 'q' to quit." >> return Nothing

        let newB =
                if isJust key
                    then fst $ updateBoard (fromJust key) b
                    else b

        return (c == 'q', newB)

    -- TODO: not to combine impure IO with pure game logic.
    -- the player is still alive if the next move is possible
    let stillAlive = any (\d -> snd $ updateBoard d newB) universe

    unless quit
        (if stillAlive && b == newB
           then do
               liftIO $ putStrLn "Invalid move"
               playGame b
           else do
               maybeNewB <- insertNewCell newB
               maybe
                   -- no place to insert a new cell, game over
                   (liftIO $ putStrLn "Game over")
                   -- the player alives after this move
                   playGame
                   maybeNewB
        )

-- | initialize the board by puting two cells randomly
--   into the board. According to the original game,
--   we have 90% probability of getting a cell of value 2,
--   and 10% probability of getting a cell of value 4.
initGame :: (RandomGen g) => RandT g IO Board
initGame =
    -- insert two cells and return the resulting board
    -- here we can safely assume that the board has at least two empty cells
    -- so that we can never have Nothing on the LHS
    liftM fromJust
              (insertNewCell initBoard
           >>= insertNewCell . fromJust)


-- | try to insert a new cell randomly
insertNewCell :: (RandomGen g) => Board -> RandT g IO (Maybe Board)
insertNewCell b = do
    let availableCells = blankCells b

    if null availableCells
       then return Nothing
       else do
           choice <- getRandomR (0, length availableCells - 1)
           let (row,col) = availableCells !! choice

           r <- getRandom
           let value = if r < (0.9 :: Float) then 2 else 4
           return $ Just (replace row (replace col value (b !! row)) b)

-- | replace the i-th element in a list
replace :: Int -> a -> [a] -> [a]
replace pos v xs = as ++ v : bs
    where
        (as, _:bs) = splitAt pos xs
