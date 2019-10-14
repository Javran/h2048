{-# LANGUAGE TupleSections #-}
module Game.H2048.NewCoreSpec where

import Data.Bifunctor
import Test.Hspec

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad

import Game.H2048.NewCore

listToGameBoard :: (Int, Int) -> [[Int]] -> GameBoard
listToGameBoard (rows,cols) grid =
    M.fromList $ zip coords (convert <$> concat grid) >>= unwrap
  where
    coords = (,) <$> [0..rows-1] <*> [0..cols-1]
    convert v =
      if v == 0
        then Nothing
        else Just (intToCell v)
    unwrap (coord, m) = case m of
      Nothing -> []
      Just m' -> [(coord, m')]

gameBoardToList :: (Int, Int) -> GameBoard -> [[Int]]
gameBoardToList (rows,cols) gb =
    (\r -> convert r <$> [0..cols-1]) <$> [0..rows-1]
  where
    convert rInd cInd = case M.lookup (rInd,cInd) gb of
      Nothing -> 0
      Just c -> cellToInt c

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
      testCase DDown $
        fmap (\c -> fmap (,c) [2,1,0]) [0..4]
      testCase DLeft $
        fmap (\r -> fmap (r,) [0..4]) [0..2]
      testCase DRight $
        fmap (\r -> fmap (r,) [4,3..0]) [0..2]

  describe "applyMove" $
    describe "examples" $ do
      let toGB = listToGameBoard (_grDim gr)
          -- used for testing while describing board as a list
          testCase listBd dir expected =
            (fmap . first) (gameBoardToList (_grDim gr))
              (applyMove gr dir (toGB listBd))
              `shouldBe` expected
          -- for making it convenient to test for multiple
          -- directions on the same board configuration.
          testCases descPref listBd expects =
            forM_ expects $ \(dir, expectedListBd) ->
              specify (descPref <> ", " <> show dir) $
                testCase listBd dir expectedListBd

      testCases "case #0"
        [ [2,2,2,2]
        , [2,4,4,2]
        , [2,4,4,2]
        , [2,2,2,2]
        ]
        [ ( DUp
          , Just ([ [4,2,2,4]
                  , [4,8,8,4]
                  , [0,2,2,0]
                  , [0,0,0,0]
                  ], 32))
        , ( DDown
          , Just ([ [0,0,0,0]
                  , [0,2,2,0]
                  , [4,8,8,4]
                  , [4,2,2,4]
                  ], 32))
        , ( DLeft
          , Just ([ [4,4,0,0]
                  , [2,8,2,0]
                  , [2,8,2,0]
                  , [4,4,0,0]
                  ], 32))
        , ( DRight
          , Just ([ [0,0,4,4]
                  , [0,2,8,2]
                  , [0,2,8,2]
                  , [0,0,4,4]
                  ], 32))
        ]
      testCases "case #1"
        [ [2,2,4,4]
        , [2,2,4,4]
        , [8,8,0,0]
        , [8,8,0,0]
        ]
        [ ( DUp
          , Just ([ [ 4, 4 ,8, 8]
                  , [16,16, 0, 0]
                  , [ 0, 0, 0, 0]
                  , [ 0, 0, 0, 0]
                  ], 56))
        , ( DDown
          , Just ([ [ 0, 0, 0, 0]
                  , [ 0, 0, 0, 0]
                  , [ 4, 4, 0, 0]
                  , [16,16, 8, 8]
                  ], 56))
        , ( DLeft
          , Just ([ [ 4, 8, 0, 0]
                  , [ 4, 8, 0, 0]
                  , [16, 0, 0, 0]
                  , [16, 0, 0, 0]
                  ], 56))
        , ( DRight
          , Just ([ [ 0, 0, 4, 8]
                  , [ 0, 0, 4 ,8]
                  , [ 0, 0, 0,16]
                  , [ 0, 0, 0,16]
                  ], 56))
        ]
