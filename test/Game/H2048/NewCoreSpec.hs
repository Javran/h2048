module Game.H2048.NewCoreSpec where

import Test.Hspec

import Game.H2048.NewCore

spec :: Spec
spec =
  describe "merge" $ do
    specify "no merge" $ do
      merge (Cell 1) (Cell 2) `shouldBe` Nothing
      merge (Cell 6) (Cell 5) `shouldBe` Nothing
    specify "merges" $ do
      merge (Cell 1) (Cell 1) `shouldBe` Just (Cell 2)
      merge (Cell 4) (Cell 4) `shouldBe` Just (Cell 5)
