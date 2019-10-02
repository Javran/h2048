{-# LANGUAGE OverloadedStrings #-}
module Game.H2048.UI.Brick where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.Bits
import Data.Functor
import Data.List
import Data.String
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

import Game.H2048.Core

data RName = RBoard deriving (Eq, Ord)

type AppState = (Board, Int {- for tracking total score -})

valToTier :: Int -> Int
valToTier = countTrailingZeros -- tier starting from 1

tierAttr :: Int -> AttrName
tierAttr = ("tier" <>) . fromString . show

boardSample :: Board
boardSample = mkBoard
  [ [1,2,4,8]
  , [16,32,64,128]
  , [256,512,1024,2048]
  , [0,0,0,0]
  ]

boardWidget :: AppState -> Widget RName
boardWidget (bdOpaque, _) =
    joinBorders
    . border
    $ grid
  where
    bd = fromBoard bdOpaque
    grid =
      hLimit (hMax*4+3) $ vBox (intersperse hBorder (row <$> [0..3]))
    row :: Int -> Widget RName
    row r =
      vLimit 1 $
        hBox (intersperse vBorder (cell r <$> [0..3]))
    contentSample = " 2048 " :: String
    hMax = length contentSample
    cell :: Int -> Int -> Widget RName
    cell r c =
      vLimit 1 . hLimit hMax $ cellW
      where
        val = bd !! r !! c
        cellW =
          if val == 0
            then fill ' '
            else
              withAttr (tierAttr . valToTier $ val)
              . padLeft Max
              $ str (show (bd !! r !! c) <> " ")

ui :: AppState -> Widget RName
ui s@(_,score) = center $
  hCenter (boardWidget s)
  <=> hCenter (str $ "Current Score: " <> show score)
  <=> hCenter (str "i / k / j / l / arrow keys to move, q to quit.")

main :: IO ()
main = do
  let app =
        App
        { appDraw = \s -> [ui s]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = pure
        , appAttrMap =
            const $
              attrMap defAttr $
                zip (tierAttr <$> [1..])
                    [ fg (ISOColor 7) `withStyle` dim
                    , fg (ISOColor 6) `withStyle` dim
                    , fg (ISOColor 3) `withStyle` dim
                    , fg (ISOColor 2) `withStyle` dim
                    , fg (ISOColor 1) `withStyle` dim
                    , fg (ISOColor 7) `withStyle` bold
                    , fg (ISOColor 4) `withStyle` bold
                    , fg (ISOColor 6) `withStyle` bold
                    , fg (ISOColor 2) `withStyle` bold
                    , fg (ISOColor 1) `withStyle` bold
                    , fg (ISOColor 3) `withStyle` bold
                    ]
        , appChooseCursor = neverShowCursor
        }
      initState = (boardSample, 0)
  void $ defaultMain app initState
