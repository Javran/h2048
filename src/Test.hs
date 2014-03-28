import Test.HUnit

import Control.Arrow
import Control.Monad
import Control.Monad.Writer
import System.Exit

import System.Game.H2048.Core

-- | the expected behavior of 'compactLine'
compactLineTestcases :: [ ((Line, Int) , Line) ]
compactLineTestcases =
    --  < expected  >   < input >
    [ ( ([0,0,0,0],0),  [0,0,0,0] )
    , ( ([2,0,0,0],2),  [1,0,0,1] )
    , ( ([4,4,0,0],8),  [2,2,2,2] )
    , ( ([2,8,2,0],8),  [2,4,4,2] )
    , ( ([2,4,8,2],0),  [2,4,8,2] )
    , ( ([2,0,0,0],2),  [0,1,0,1] )
    , ( ([1,2,0,0],0),  [1,0,2,0] )
    ]

compactLineTests :: Test
compactLineTests = TestLabel "compactLine" . TestList . map
        (\(expected, inp) ->
             expected ~=?
             second getSum (runWriter (compactLine inp)))
        $ compactLineTestcases

-- | the expected behavior of 'gameState'
gameStateTestcases :: [ (String, GameState, Board) ]
gameStateTestcases =
    [ ( "trivial win, alive",
        WinAlive , [ [ 2048, 2048, 2048, 2048 ]
                   , [    0,    0,    0,    0 ]
                   , [    0,    0,    0,    0 ]
                   , [    0,    0,    0,    0 ] ] )
    , ( "more than 2048 (might not happen in practice)",
        Win      , [ [  256,    2,    4,    8 ]
                   , [   16,   32,   64,  128 ]
                   , [  256,  512, 1024,    8 ]
                   , [   16, 4096,  128,   64 ] ] )
    , ( "no more move but win",
        Win      , [ [    2,    4,    2,    4 ]
                   , [    4, 2048,    4,    2 ]
                   , [    2,    4,    2,    4 ]
                   , [    4,    2,    4,    2 ] ] )
    , ( "trivial alive",
        Alive    , [ [    2,    0,    0,    8 ]
                   , [    4,    0,    0,    8 ]
                   , [    4,    0,    0,    8 ]
                   , [ 1024,  512,  128,   64 ] ] )
    , ( "no empty cell but still alive",
        Alive    , [ [  512,  128,  512,  128 ]
                   , [  128,  512,  128,  512 ]
                   , [  512,  128,  512,  128 ]
                   , [  128,  512,  128,  128 ] ] )
    , ( "trivial lose 1",
        Lose     , [ [    2,    4,    2,    4 ]
                   , [    4,    2,    4,    2 ]
                   , [    2,    4,    2,    4 ]
                   , [    4,    2,    4,    2 ] ] )
    , ( "trivial lose 2",
        Lose     , [ [    2,    8,   32,    2 ]
                   , [    4,    2,    8,    4 ]
                   , [   32,   16,  128,   16 ]
                   , [    4,    8,    4,    2 ] ] )
    ]

gameStateTests :: Test
gameStateTests = TestLabel "gameState" . TestList . map
        (\(lbl, expected, inp) ->
             TestLabel lbl (expected ~=? gameState inp))
        $ gameStateTestcases

-- | run testcase and quit immediately after a failure
runTestFailIm :: Test -> IO ()
runTestFailIm t = do
    cs <- runTestTT t
    print cs
    when (errors cs > 0 || failures cs > 0)
         exitFailure

main :: IO ()
main = mapM_ runTestFailIm
       [ compactLineTests
       , gameStateTests
       ]
