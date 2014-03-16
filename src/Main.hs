module Main
where

import Control.Monad
import Control.Monad.Random
import System.IO

import Game.H2048.Core

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    g <- newStdGen
    void $ evalRandT (initGame >>= playGame) g
