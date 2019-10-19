{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Game.H2048.UI.Brick where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.Bits
import Data.Functor
import Data.List
import Data.String
import Data.Maybe
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Control.Monad.IO.Class

import Game.H2048.Core

data RName = RBoard deriving (Eq, Ord)

type AppState = (Board, Int {- for tracking total score -})

valToTier :: Int -> Int
valToTier = countTrailingZeros -- tier starting from 1

tierAttr :: Int -> AttrName
tierAttr = ("tier" <>) . fromString . show

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
              $ str (show val <> " ")

ui :: AppState -> Widget RName
ui s@(bd,score) =
    center $
      hCenter (boardWidget s)
      <=> hCenter (str $ "Current Score: " <> show score)
      <=> hCenter (str ctrlHelpMsg)
  where
    GS {hasWon, isAlive} = gameState bd
    moveHelp = "i / k / j / l / arrow keys to move, "
    commonHelp = "q to quit, r to restart."
    {- TODO: this starts getting awkward, perhaps time to split the widget. -}
    ctrlHelpMsg =
      if not isAlive
        then
          (if hasWon then "You won, but no more moves. " else "No more moves, game over. ")
          <> commonHelp
        else
          (if hasWon then "You've won! " else "")
          <> moveHelp <> commonHelp

handleEvent :: AppState -> BrickEvent RName e -> EventM RName (Next AppState)
handleEvent s@(bd,score) e = case e of
  VtyEvent (EvKey (KChar 'q') []) -> halt s
  VtyEvent (EvKey (KChar 'r') []) ->
    liftIO initGameBoard >>= continue
  VtyEvent (EvKey k [])
    | Just dir <- getMove k -> case updateBoard dir bd of
        Nothing -> continue s
        Just (bd', awarded) -> do
          bd'' <- fromJust <$> liftIO (insertNewCell bd')
          continue (bd'', score+awarded)
  _ -> continue s

getMove :: Key -> Maybe Dir
getMove KUp = Just DUp
getMove KDown = Just DDown
getMove KLeft = Just DLeft
getMove KRight = Just DRight
getMove (KChar 'i') = Just DUp
getMove (KChar 'k') = Just DDown
getMove (KChar 'j') = Just DLeft
getMove (KChar 'l') = Just DRight
getMove _ = Nothing

main :: IO ()
main = do
  initGb <- initGameBoard
  let app =
        App
        { appDraw = \s -> [ui s]
        , appHandleEvent = handleEvent
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
      initState = initGb
  void $ defaultMain app initState
