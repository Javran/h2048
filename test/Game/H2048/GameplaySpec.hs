module Game.H2048.GameplaySpec
  ( spec
  ) where

import Test.Hspec
import System.Random.TF

import Game.H2048.NewCoreSpec (listToGameBoard)
import Game.H2048.Gameplay

spec :: Spec
spec =
  describe "isAlive & hasWon" $ do
    let -- following pairs stand for (<won?>, <alive?>)
        gWonAlive = (True, True)
        gWonOver = (True, False)
        gAlive = (False, True)
        gLose = (False, False)
        testCase boardList expected = do
          let gp =
                (mkGameplay
                  (mkTFGen 0xDEADBEEF)
                  standardGameRule)
                { _gpBoard =
                    listToGameBoard
                      (_grDim . _gpRule $ gp)
                      boardList
                }
          (hasWon gp, isAlive gp) `shouldBe` expected
    specify "trivial win, alive" $
      testCase
        [ [ 2048, 2048, 2048, 2048 ]
        , [    0,    0,    0,    0 ]
        , [    0,    0,    0,    0 ]
        , [    0,    0,    0,    0 ]
        ] gWonAlive
    specify "more than 2048 (might not happen in practice)" $
      testCase
        [ [  256,    2,    4,    8 ]
        , [   16,   32,   64,  128 ]
        , [  256,  512, 1024,    8 ]
        , [   16, 4096,  128,   64 ]
        ] gWonOver
    specify  "no more move but win" $
      testCase
        [ [    2,    4,    2,    4 ]
        , [    4, 2048,    4,    2 ]
        , [    2,    4,    2,    4 ]
        , [    4,    2,    4,    2 ]
        ] gWonOver
    specify "trivial alive" $
      testCase
        [ [    2,    0,    0,    8 ]
        , [    4,    0,    0,    8 ]
        , [    4,    0,    0,    8 ]
        , [ 1024,  512,  128,   64 ]
        ] gAlive
    specify "no empty cell but still alive" $
      testCase
        [ [  512,  128,  512,  128 ]
        , [  128,  512,  128,  512 ]
        , [  512,  128,  512,  128 ]
        , [  128,  512,  128,  128 ]
        ] gAlive
    specify "trivial lose 1" $
      testCase
        [ [    2,    4,    2,    4 ]
        , [    4,    2,    4,    2 ]
        , [    2,    4,    2,    4 ]
        , [    4,    2,    4,    2 ]
        ] gLose
    specify "trivial lose 2" $
      testCase
        [ [    2,    8,   32,    2 ]
        , [    4,    2,    8,    4 ]
        , [   32,   16,  128,   16 ]
        , [    4,    8,    4,    2 ]
        ] gLose
