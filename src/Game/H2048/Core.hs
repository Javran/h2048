module Game.H2048.Core
-- TODO: minimize export
where

import Control.Arrow
-- import Control.Lens hiding (universe)
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
        (board',score) = rTransL >>> map compactLine >>> unzip >>> first rTransR >>> second sum $ board
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

-- | the board is dead if there is no valid move
isDead :: Board -> Bool
isDead b = all (\d -> isNothing $ updateBoard d b) universe

-- TODO: move CLI logic to somewhere else

-- | play game on a given board until user quits or game ends
playGame :: (RandomGen g) => (Board, Int) -> RandT g IO ()
playGame (b,score) = do
        -- when game over
    let gameOver (b',score') = do
            drawBoard b'
            putStrLn "Game over"
            _ <- printf "Final score: %d\n" score'
            hFlush stdout
        -- handle user move, print the board together with current score,
        -- return the next user move:
        -- * return Nothing only if user has pressed "q"
        -- * return Just <key>   if one of "ijkl" is pressed
        handleUserMove = do
            drawBoard b
            _ <- printf "Current score: %d\n" score
            hFlush stdout
            c <- getChar
            putStrLn ""
            hFlush stdout

            -- TODO: customizable
            maybeKey <- case c of
                     'q' -> return Nothing
                     'i' -> putStrLn "Up"    >> return (Just DUp)
                     'k' -> putStrLn "Down"  >> return (Just DDown)
                     'j' -> putStrLn "Left"  >> return (Just DLeft)
                     'l' -> putStrLn "Right" >> return (Just DRight)
                     _ -> do
                             putStrLn "'i'/'k'/'j'/'l' to move, 'q' to quit."
                             return $ error "Unreachable code: unhandled invalid user input"

            if c `elem` "qijkl"
               -- user will not be on this branch
               -- if an invalid key is pressed
               then return maybeKey
               -- user will be trapped in "handleUserMove" unless
               -- a valid key is given. So `return undefined` can never be reached
               else handleUserMove
        handleGame =
            maybe
                -- user quit
                (return ())
                -- user next move
                  -- 1. update the board according to user move
                ((`updateBoard` b) >>>
                  -- 2. the update might succeed / fail
                  maybe
                         -- 2(a). the move is invalid, try again
                         (liftIO (putStrLn "Invalid move") >> playGame (b,score))
                         -- 2(b). on success, insert new cell
                         (\ result -> do
                              -- should always succeed
                              -- because when a successful move is done
                              -- there is at least one empty cell in the board
                              (Just newB) <- insertNewCell (brBoard result)
                              -- keep going, accumulate score
                              playGame (newB, score + brScore result)))

    if isDead b
       then liftIO $ gameOver (b,score)
       else liftIO handleUserMove >>= handleGame

-- | initialize the board by puting two cells randomly
--   into the board. According to the original game,
--   we have 90% probability of getting a cell of value 2,
--   and 10% probability of getting a cell of value 4.
initGame :: (RandomGen g) => RandT g IO (Board, Int)
initGame =
    -- insert two cells and return the resulting board
    -- here we can safely assume that the board has at least two empty cells
    -- so that we can never have Nothing on the LHS
    liftM ( (\x -> (x,0)) . fromJust) (insertNewCell initBoard >>= (insertNewCell . fromJust))

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
-- replace pos v xs = xs & (ix pos) .~ v
replace pos v xs = as ++ v : bs
    where
        (as, _:bs) = splitAt pos xs
