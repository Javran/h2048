module Game.H2048.CoreSpec where

import Test.Hspec

import Game.H2048.Core

spec :: Spec
spec = do
  describe "compactLine" $
    specify "examples" $ do
      let clTest inp expected =
              compactLine' (mkLine inp)
                `shouldBe` (eScore, mkLine eLine)
            where
              (eScore, eLine) = expected
      clTest [0,0,0,0] (0,[0,0,0,0])
      clTest [1,0,0,1] (2,[2,0,0,0])
      clTest [2,2,2,2] (8,[4,4,0,0])
      clTest [2,4,4,2] (8,[2,8,2,0])
      clTest [2,4,8,2] (0,[2,4,8,2])
      clTest [0,1,0,1] (2,[2,0,0,0])
      clTest [1,0,2,0] (0,[1,2,0,0])

  describe "gameState" $ do
    let gWonAlive = GS True True
        gWon = GS True False
        gAlive = GS False True
        gLose = GS False False
        gameState' = gameState . mkBoard
    specify "trivial win, alive" $
      gameState'
        [ [ 2048, 2048, 2048, 2048 ]
        , [    0,    0,    0,    0 ]
        , [    0,    0,    0,    0 ]
        , [    0,    0,    0,    0 ]
        ] `shouldBe` gWonAlive
    specify "more than 2048 (might not happen in practice)" $
      gameState'
        [ [  256,    2,    4,    8 ]
        , [   16,   32,   64,  128 ]
        , [  256,  512, 1024,    8 ]
        , [   16, 4096,  128,   64 ]
        ] `shouldBe` gWon
    specify  "no more move but win" $
      gameState'
        [ [    2,    4,    2,    4 ]
        , [    4, 2048,    4,    2 ]
        , [    2,    4,    2,    4 ]
        , [    4,    2,    4,    2 ]
        ] `shouldBe` gWon
    specify "trivial alive" $
      gameState'
        [ [    2,    0,    0,    8 ]
        , [    4,    0,    0,    8 ]
        , [    4,    0,    0,    8 ]
        , [ 1024,  512,  128,   64 ]
        ] `shouldBe` gAlive
    specify "no empty cell but still alive" $
      gameState'
        [ [  512,  128,  512,  128 ]
        , [  128,  512,  128,  512 ]
        , [  512,  128,  512,  128 ]
        , [  128,  512,  128,  128 ]
        ] `shouldBe` gAlive
    specify "trivial lose 1" $
      gameState'
        [ [    2,    4,    2,    4 ]
        , [    4,    2,    4,    2 ]
        , [    2,    4,    2,    4 ]
        , [    4,    2,    4,    2 ]
        ] `shouldBe` gLose
    specify "trivial lose 2" $
      gameState'
        [ [    2,    8,   32,    2 ]
        , [    4,    2,    8,    4 ]
        , [   32,   16,  128,   16 ]
        , [    4,    8,    4,    2 ]
        ] `shouldBe` gLose
