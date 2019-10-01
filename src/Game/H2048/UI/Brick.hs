module Game.H2048.UI.Brick where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.List
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events

data RName = RBoard deriving (Eq, Ord)

boardWidget :: () -> Widget RName
boardWidget _ =
    center
    . joinBorders
    . border
    $ grid
  where
    grid =
      hLimit (hMax*4+3) $ vBox (intersperse hBorder (replicate 4 row))
    row =
      vLimit 1 $
        hBox (intersperse vBorder (replicate 4 sampleCell))
    hMax = length "2048"
    sampleCell =
      vLimit 1
      . hLimit hMax
      . center
      $ str "2048"

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
      initState = ()
  defaultMain app initState
