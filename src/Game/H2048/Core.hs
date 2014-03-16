module Game.H2048.Core
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.List
import Data.Maybe
import System.IO
import Text.Printf

type Board = [[Int]]
type Line  =  [Int]

data Dir = DUp
         | DDown
         | DLeft
         | DRight
         deriving (Enum, Eq, Ord, Show)

initBoard :: Board
initBoard = replicate 4 $ replicate 4 0

drawBoard :: Board -> IO ()
drawBoard board = do
    putStrLn horizSeparator
    mapM_ (\x -> drawRow x >> putStrLn horizSeparator) board
    where
        cellWidth = length " 2048 "
        horizSeparator' =
            intercalate "+" (replicate 4 (replicate cellWidth '-'))
        horizSeparator = "+" ++ horizSeparator' ++ "+"

        drawRow :: [Int] -> IO ()
        drawRow row = do
            putChar '|'
            mapM_ (printf " %4d |") row
            putChar '\n'

compactLine :: Line -> Line
compactLine rawXs =
    unpack packedResult
    where
        pack = filter (/= 0)
        unpack = padLeft 4

        xs = pack rawXs

        (rLine, rBoard) = foldl update initState xs

        packedResult =
            if isNothing rBoard
               then rLine
               else rLine ++ [fromJust rBoard]

        initState :: (Line, Maybe Int)
        initState = ([], Nothing)

        update :: (Line, Maybe Int) -> Int -> (Line, Maybe Int)
        update (curLine, prevBoardM) curBoard =
            case prevBoardM of
              Nothing ->
                  (curLine, Just curBoard)
              Just prevBoard ->
                  if prevBoard == curBoard
                     then (curLine ++ [curBoard * 2], Nothing)
                     else (curLine ++ [prevBoard], Just curBoard)

        padLeft :: Int -> Line -> Line
        padLeft l line =
            if length line < l
                then line ++ replicate (4 - length line) 0
                else line

updateBoard :: Dir -> Board -> (Board, Bool)
updateBoard d cell = (cell', cell /= cell')
    where
        cell' :: Board
        cell' = bracketF rTransL rTransR (map compactLine) cell
        rTrans :: [Board -> Board]
        rTrans =
            case d of
              DLeft  -> []
              DRight -> [map reverse]
              DUp    -> [transpose]
              DDown  -> [transpose, map reverse]

        rTransL = foldl (flip (.)) id rTrans
        rTransR = foldr       (.)  id rTrans

bracketF :: (c -> a) -> (b -> d)
          -> (a -> b)
          ->  c -> d
bracketF openF closeF f = closeF . f . openF

blankCells :: Board -> [(Int, Int)]
blankCells b = map (\(row, (col, _)) -> (row,col) ) blankCells'
    where
        blankCells' = filter ((== 0) . snd . snd) linearBoard
        linearBoard = concat $ zipWith tagRow [0..] colTagged
        tagRow row = map ( (,) row )
        colTagged = map (zip [0..]) b

playGame :: (RandomGen g) => Board -> RandT g IO ()
playGame b = do
    (quit,newB) <- liftIO $ do
        drawBoard b
        hFlush stdout

        c <- getChar

        putStrLn ""
        hFlush stdout

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

    unless quit
        (if b == newB
           then do
               liftIO $ putStrLn "Invalid move"
               playGame b
           else do
               maybeNewB <- insertNewCell newB
               maybe
                   (liftIO $ putStrLn "Game over")
                   playGame
                   maybeNewB
        )

initGame :: (RandomGen g) => RandT g IO Board
initGame = do
    (Just b1) <- insertNewCell initBoard
    (Just b2) <- insertNewCell b1
    return b2

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

replace :: Int -> a -> [a] -> [a]
replace pos v xs = as ++ v : bs
    where
        (as, _:bs) = splitAt pos xs
