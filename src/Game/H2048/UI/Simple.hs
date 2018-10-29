{-|
  Module      : Game.H2048.UI.Simple
  Copyright   : (c) 2014 Javran Cheng
  License     : MIT
  Maintainer  : Javran.C@gmail.com
  Stability   : experimental
  Portability : POSIX

A simple CLI implemention of Game 2048

-}
module Game.H2048.UI.Simple
  ( drawBoard
  , playGame
  , mainSimple
  )
where

import Game.H2048.Core

import Data.List
import Text.Printf
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Arrow
import System.IO

-- a simple UI implemented by outputing strings

-- | simple help string
helpString :: String
helpString =  "'i'/'k'/'j'/'l' to move, 'q' to quit."

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

        -- pretty string for a cell (without border)
        prettyCell c = if c == 0
                           then replicate cellWidth ' '
                           else printf " %4d " c

        drawRow :: [Int] -> IO ()
        drawRow row = do
            -- prints "| <cell1> | <cell2> | ... |"
            putChar '|'
            mapM_ (prettyCell >>> putStr >>> (>> putChar '|') ) row
            putChar '\n'
            putStrLn horizSeparator

-- | play game on a given board until user quits or game ends
playGame :: (RandomGen g) => (Board, Int) -> RandT g IO ()
playGame (b,score) = do
        -- when game over
    let endGame (b',score') win = do
            drawBoard b'
            putStrLn $ if win
                           then "You win"
                           else "Game over"
            _ <- printf "Final score: %d\n" score'
            hFlush stdout
        -- handle user move, print the board together with current score,
        -- return the next user move:
        -- * return Nothing only if user has pressed "q"
        -- * return Just <key>   if one of "ijkl" is pressed
        handleUserMove win = do
            let scoreFormat =
                    if win
                        then "You win, current score: %d\n"
                        else "Current score: %d\n"
            drawBoard b
            _ <- printf scoreFormat score
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
                             putStrLn helpString
                             return $ error "Unreachable code: unhandled invalid user input"

            if c `elem` "qijkl"
               -- user will not be on this branch
               -- if an invalid key is pressed
               then return maybeKey
               -- user will be trapped in "handleUserMove" unless
               -- a valid key is given. So the error above (the wildcard case)
               -- can never be reached
               else handleUserMove win
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
                         (\(newBoard, scoreObtained) -> do
                              -- should always succeed
                              -- because when a successful move is done
                              -- there is at least one empty cell in the board
                              (Just newB) <- insertNewCell newBoard
                              -- keep going, accumulate score
                              playGame (newB, score + scoreObtained)))

    case gameState b of
      Win ->
          liftIO $ endGame (b,score) True
      Lose ->
          liftIO $ endGame (b,score) False
      WinAlive ->
          liftIO (handleUserMove True ) >>= handleGame
      Alive ->
          liftIO (handleUserMove False) >>= handleGame

-- | the entry of Simple UI
mainSimple :: IO ()
mainSimple = do
    bfMod <- hGetBuffering stdin
    -- no buffering - don't wait for the "enter"
    hSetBuffering stdin NoBuffering
    g <- newStdGen
    -- show some helpful messages
    -- whether the user has read the README or not :)
    putStrLn helpString
    -- initialize game based on the random seed
    _ <- evalRandT (initGameBoard >>= playGame) g
    -- restoring buffering setting
    hSetBuffering stdin bfMod
