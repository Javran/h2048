import Test.HUnit

import Control.Arrow
import Control.Monad
import Control.Monad.Writer
import System.Exit

import Game.H2048.Core

testcases :: [ ((Line, Int) , Line) ]
testcases =
    --  < expected  >   < input >
    [ ( ([0,0,0,0],0),  [0,0,0,0] )
    , ( ([2,0,0,0],2),  [1,0,0,1] )
    , ( ([4,4,0,0],8),  [2,2,2,2] )
    , ( ([2,8,2,0],8),  [2,4,4,2] )
    , ( ([2,4,8,2],0),  [2,4,8,2] )
    , ( ([2,0,0,0],2),  [0,1,0,1] )
    , ( ([1,2,0,0],0),  [1,0,2,0] )
    ]

tests :: Test
tests = TestList $ map
        (\(expected, inp) ->
             expected ~=?
             second getSum (runWriter (compactLine inp)))
        testcases

main :: IO ()
main = do
    cs <- runTestTT tests
    print cs
    when (errors cs > 0 || failures cs > 0)
         exitFailure
