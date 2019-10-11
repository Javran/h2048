{-# LANGUAGE TupleSections #-}
module Game.H2048.NewCoreSpec where

import Data.Bifunctor
import Test.Hspec

import qualified Data.IntMap as IM
import qualified Data.Vector as V

import Game.H2048.NewCore

spec :: Spec
spec = do
  let gr = standardGameRule
  describe "merge" $ do
    specify "no merge" $ do
      merge (Cell 1) (Cell 2) `shouldBe` Nothing
      merge (Cell 6) (Cell 5) `shouldBe` Nothing
    specify "merges" $ do
      merge (Cell 1) (Cell 1) `shouldBe` Just (Cell 2)
      merge (Cell 4) (Cell 4) `shouldBe` Just (Cell 5)
  describe "mergeWithScore" $
    specify "examples" $ do
      mergeWithScore gr (Cell 1) (Cell 1)
        `shouldBe` Just (Cell 2, 4)
      mergeWithScore gr (Cell 4) (Cell 4)
        `shouldBe` Just (Cell 5, 32)
  describe "mergeLines" $
    specify "examples" $ do
      let mergeLine' =
            first (fmap cellToInt)
            . mergeLine gr
            . fmap intToCell
      mergeLine' [] `shouldBe` ([], 0)
      mergeLine' [2,2,4] `shouldBe` ([4,4], 4)
      mergeLine' [4,4,4] `shouldBe` ([8,4], 8)
      mergeLine' [4,8,8] `shouldBe` ([4,16], 16)
      mergeLine' [4,2,2,8] `shouldBe` ([4,4,8], 4)
      mergeLine' [2,2,2,2] `shouldBe` ([4,4], 8)
  describe "computeDistrib" $
    specify "examples" $
      computeDistrib (IM.fromList [(1,2),(2,5),(3,7),(4,6)])
        `shouldBe` V.fromList [(1,2),(2,7),(3,14),(4,20)]
  describe "dirToCoordsGroups" $ do
    specify "standard game examples" $ do
      let testCase dir expected =
            dirToCoordsGroups gr dir
              `shouldBe` expected
      testCase DUp $
        fmap (\c -> fmap (,c) [0..3]) [0..3]
      testCase DDown $
        fmap (\c -> fmap (,c) [3,2..0]) [0..3]
      testCase DLeft $
        fmap (\r -> fmap (r,) [0..3]) [0..3]
      testCase DRight $
        fmap (\r -> fmap (r,) [3,2..0]) [0..3]
    specify "non-square examples" $ do
      let testCase dir expected =
            dirToCoordsGroups (gr {_grDim = (3,5)}) dir
              `shouldBe` expected
      testCase DUp $
        fmap (\c -> fmap (,c) [0..2]) [0..4]
      testCase DRight $
        fmap (\r -> fmap (r,) [4,3..0]) [0..2]
