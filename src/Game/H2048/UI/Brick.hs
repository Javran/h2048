module Game.H2048.UI.Brick where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.List
import Data.Functor
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events

import Game.H2048.Core

data RName = RBoard deriving (Eq, Ord)

boardSample :: Board
boardSample = mkBoard
  [ [1,2,4,8]
  , [16,32,64,128]
  , [256,512,1024,2048]
  , [0,0,0,0]
  ]

boardWidget :: Board -> Widget RName
boardWidget bdOpaque =
    center
    . joinBorders
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
    contentSample = " 2048 "
    hMax = length contentSample
    cell :: Int -> Int -> Widget RName
    cell r c =
      vLimit 1 . hLimit hMax $ cellW
      where
        val = bd !! r !! c
        cellW =
          if val == 0
            then fill ' '
            else padLeft Max
              $ str (show (bd !! r !! c) <> " ")

main :: IO ()
main = do
  let app =
        App
        { appDraw = \s -> [boardWidget s]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = pure
        , appAttrMap = const $ attrMap defAttr []
        , appChooseCursor = neverShowCursor
        }
      initState = boardSample
  void $ defaultMain app initState
