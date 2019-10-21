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
  , main
  )
where

import Control.Arrow
import Data.Function
import Data.Functor
import Data.List
import System.IO
import System.Random.TF
import Text.Printf

import qualified Data.Map.Strict as M

import Game.H2048.Gameplay

-- a simple command line interface implemented by simply outputing strings

-- | simple help string
helpString :: String
helpString =  "'i'/'k'/'j'/'l' to move, 'q' to quit."

-- | pretty print the board to stdout
drawBoard :: Gameplay -> IO ()
drawBoard gp =
    {-
     a cell will be represented in the output as following:

       +-----+
       | xxx |
       +-----+

     the pretty-printing strategy is to print the first line
     and for each row in the board:

     * print the leftmost "| "
     * let each cell in the row print " <number> |"
     * finalize this line by printing out the horizontal "+--+--+..."
    -}
    putStrLn horizSeparator >>
    mapM_ drawRow [0 .. rowCount - 1]
  where
    bd = _gpBoard gp
    (rowCount, colCount) =  _grDim . _gpRule $ gp
    cellWidth = length " 2048 "
    -- build up the separator: "+--+--+....+"
    horizSeparator' = intercalate "+" (replicate 4 (replicate cellWidth '-'))
    horizSeparator = "+" ++ horizSeparator' ++ "+"

    -- pretty string for a cell (without border)
    prettyCell :: Int -> Int -> String
    prettyCell r c = case bd M.!? (r,c) of
      Nothing -> replicate cellWidth ' '
      Just cell -> printf " %4d " (cellToInt cell)

    drawRow :: Int -> IO ()
    drawRow rowInd = do
      -- prints "| <cell1> | <cell2> | ... |"
      putChar '|'
      mapM_ (prettyCell rowInd >>> putStr >>> (>> putChar '|')) [0 .. colCount - 1]
      putChar '\n'
      putStrLn horizSeparator

-- | play game on a given board until user quits or game ends
playGame :: IO Gameplay
playGame = do
  g <- newTFGen
  let initState = mkGameplay g standardGameRule
  gameLoop (newGame initState)
 where
   gameLoop gp = do
     drawBoard gp
     if isAlive gp
       then processUserMove gp
       else endGame gp

   endGame gp = do
     putStrLn $ if hasWon gp then "You won" else "Game over"
     _ <- printf "Final score: %d\n" (_gpScore gp)
     gp <$ hFlush stdout

   -- handle user move, print the board together with current score,
   -- return the next user move:
   -- + return Nothing only if user has pressed "q"
   -- + return Just <key>   if one of "ijkl" is pressed
   processUserMove :: Gameplay -> IO Gameplay
   processUserMove gp = fix $ \redo -> do
     let scoreFormat =
           if hasWon gp
             then "You win, current score: %d\n"
             else "Current score: %d\n"
     printf scoreFormat (_gpScore gp)
     hFlush stdout
     c <- getChar
     putStrLn ""
     hFlush stdout
     case c of
       'q' -> pure gp
       'i' -> putStrLn "Up" >> handleMove gp DUp
       'k' -> putStrLn "Down" >> handleMove gp DDown
       'j' -> putStrLn "Left" >> handleMove gp DLeft
       'l' -> putStrLn "Right" >> handleMove gp DRight
       _ ->
         -- user will not be on this branch
         -- if an invalid key is pressed
         putStrLn helpString >> redo

   handleMove :: Gameplay -> Dir -> IO Gameplay
   handleMove gp dir = case stepGame dir gp of
     Nothing -> putStrLn "Invalid move" >> gameLoop gp
     Just gp' -> gameLoop gp'

-- | the entry of Simple UI
main :: IO ()
main = do
  -- turn off buffering to not wait on a newline character.
  hSetBuffering stdin NoBuffering
  -- show some helpful messages
  -- whether the user has read the README or not :)
  putStrLn helpString
  -- initialize game based on the random seed
  void playGame
