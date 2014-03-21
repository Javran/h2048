module Main
where

import Control.Monad
import Control.Monad.Random
import System.IO

import Game.H2048.Core
import Game.H2048.UI.Simple

main :: IO ()
main = do
    -- don't wait for the "enter"
    hSetBuffering stdin NoBuffering
    g <- newStdGen
    -- initialize game based on the random seed
    void $ evalRandT (initGame >>= playGame) g
