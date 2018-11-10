{-|
  Module      : Game.H2048.UI.Simple
  Copyright   : (c) 2014 Javran Cheng
  License     : MIT
  Maintainer  : Javran.C@gmail.com
  Stability   : experimental
  Portability : POSIX

A simple CLI implemention of Game 2048

-}
{-# LANGUAGE NamedFieldPuns #-}
module Game.H2048.UI.Simple
  ( drawBoard
  , playGame
  , mainSimple
  , Board
  )
where

import Game.H2048.Core
import Data.List
import Text.Printf
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Applicative
import Control.Arrow
import System.IO

-- a simple UI implemented by outputing strings

-- | simple help string
helpString :: String
helpString =  "'i'/'k'/'j'/'l' to move, 'q' to quit."

-- | pretty print the board to stdout
drawBoard :: Board -> IO ()
drawBoard bd = do
    let board = fromBoard bd
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

        drawRow row = do
            -- prints "| <cell1> | <cell2> | ... |"
            putChar '|'
            mapM_ (prettyCell >>> putStr >>> (>> putChar '|') ) row
            putChar '\n'
            putStrLn horizSeparator

-- | play game on a given board until user quits or game ends
playGame :: (MonadIO m, MonadRandom m, Alternative m) => (Board, Int) -> m ()
playGame args@(b,score) |
  GS {hasWon, isAlive} <- gameState b =
    if isAlive
      then liftIO (handleUserMove hasWon) >>= handleGame
      else liftIO (endGame args hasWon)
 where
   endGame (b',score') win = do
     drawBoard b'
     putStrLn $ if win then "You won" else "Game over"
     _ <- printf "Final score: %d\n" score'
     hFlush stdout
   -- handle user move, print the board together with current score,
   -- return the next user move:
   -- + return Nothing only if user has pressed "q"
   -- + return Just <key>   if one of "ijkl" is pressed
   handleUserMove w = fix $ \self -> do
     let scoreFormat =
           if w
             then "You win, current score: %d\n"
             else "Current score: %d\n"
     drawBoard b
     _ <- printf scoreFormat score
     hFlush stdout
     c <- getChar
     putStrLn ""
     hFlush stdout
     -- TODO: customizable
     case c of
       'q' -> pure Nothing
       'i' -> putStrLn "Up"    >> pure (Just DUp)
       'k' -> putStrLn "Down"  >> pure (Just DDown)
       'j' -> putStrLn "Left"  >> pure (Just DLeft)
       'l' -> putStrLn "Right" >> pure (Just DRight)
       _ -> do
         -- user will not be on this branch
         -- if an invalid key is pressed
         putStrLn helpString
         self
   handleGame =
     maybe
       -- user quit
       (pure ())
       -- user next move
       -- 1. update the board according to user move
       ((`updateBoard` b) >>>
         -- 2. the update might succeed / fail
         maybe
           -- 2(a). the move is invalid, try again
           (liftIO (putStrLn "Invalid move") >> playGame args)
           -- 2(b). on success, insert new cell
           (\(newBoard, scoreObtained) -> do
               -- should always succeed
               -- because when a successful move is done
               -- there is at least one empty cell in the board
               (Just newB) <- insertNewCell newBoard
               -- keep going, accumulate score
               playGame (newB, score + scoreObtained)))

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
