import Test.HUnit

import Control.Monad
import System.Exit

import Game.H2048.Core

testcases :: [ ([Int], [Int]) ]
testcases =
    [ ([0,0,0,0], [0,0,0,0])
    , ([1,0,0,1], [2,0,0,0])
    , ([2,2,2,2], [4,4,0,0])
    , ([2,4,4,2], [2,8,2,0])
    , ([2,4,8,2], [2,4,8,2])
    , ([0,1,0,1], [2,0,0,0])
    , ([1,0,2,0], [1,2,0,0])
    ]

tests :: Test
tests = TestList $ map (\(inp, expected) -> expected ~=? compactLine inp) testcases

main :: IO ()
main = do
    cs <- runTestTT tests
    print cs
    when (errors cs > 0 || failures cs > 0)
         exitFailure
