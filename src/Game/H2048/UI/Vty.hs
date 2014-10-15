{-|
  Module      : Game.H2048.UI.Vty
  Copyright   : (c) 2014 Javran Cheng
  License     : MIT
  Maintainer  : Javran.C@gmail.com
  Stability   : experimental
  Portability : POSIX

A CLI version of Game 2048 implemented using vty-ui

-}
{-# LANGUAGE OverloadedStrings #-}
module Game.H2048.UI.Vty
    ( PlayState (..)
    , mainVty
    )
where

import Graphics.Vty.Widgets.All
import Graphics.Vty
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Random
import Data.Foldable (foldMap)
import Data.IORef
import Data.Maybe

import Game.H2048.Core

-- | indicate the status of a playing session
data PlayState g = PlayState
    { psBoard  :: Board     -- ^ current board
    , psScore  :: Int       -- ^ current collected score
    , psGState :: GameState -- ^ indicate whether the game terminates
    , psRGen   ::  g        -- ^ next random generator
    }

-- | flatten a 2D list, and keep the original
--   coordinate with the actual value, each element
--   in the resulting list looks like @({value},({row index},{colum index}))@.
toIndexedBoard :: Board -> [(Int, (Int, Int))]
toIndexedBoard b = concat $ zipWith go [0..] taggedCols
    where
        -- board with each cell tagged with its col num
        taggedCols = map (zip [0..]) b
        go :: Int -> [(Int,Int)] -> [(Int,(Int,Int))]
        go row = map (\(col,val) -> (val,(row,col)))

-- | calculate colors and styles for a given number
colorize :: Int -> [(T.Text, Attr)]
colorize i = [(s,attr)]
    where
        s = if i /= 0 then (T.pack . show) i else " "
        attr = Attr (SetTo colorSty) (SetTo colorNum) Default
        (colorSty, colorNum) = fromMaybe (bold,ISOColor 3) (lookup i colorDict)
        colorDict =
            [ (   0, (  dim, ISOColor 0))
            , (   2, (  dim, ISOColor 7))
            , (   4, (  dim, ISOColor 6))
            , (   8, (  dim, ISOColor 3))
            , (  16, (  dim, ISOColor 2))
            , (  32, (  dim, ISOColor 1))
            , (  64, ( bold, ISOColor 7))
            , ( 128, ( bold, ISOColor 4))
            , ( 256, ( bold, ISOColor 6))
            , ( 512, ( bold, ISOColor 2))
            , (1024, ( bold, ISOColor 1))
            , (2048, ( bold, ISOColor 3))
            ]

-- | render UI according to the PlayState
renderGame :: PlayState g              -- ^ the PlayState
           -> [[Widget FormattedText]] -- ^ cell widgets
           -> Widget FormattedText     -- ^ status bar widget
           ->  IO ()
renderGame (PlayState bd sc gs _) items st = do
    let ixBd = toIndexedBoard bd
        renderCell v (row,col) = do
            let item = items !! row !! col
            -- for each cell, colorize it
            -- and update the corresponding widget
            item `setTextWithAttrs` colorize v
        -- the beginning of status bar
        scoreDesc = case gs of
                      Win -> "You win. Final score: "
                      Lose -> "Game over. Final score: "
                      Alive -> "Current score: "
                      WinAlive -> "You win, current score:"

    -- update table
    mapM_ (uncurry renderCell) ixBd
    -- update status bar
    setText st $ T.pack (scoreDesc ++ show sc)

-- | perform game update when a new direction is given
newDirGameUpdate :: (RandomGen g)
                 => IORef (PlayState g)      -- ^ where PlayState is held
                 -> [[Widget FormattedText]] -- ^ cell widgets
                 -> Widget FormattedText     -- ^ status bar
                 -> Dir                      -- ^ new direction
                 -> IO Bool
newDirGameUpdate psR items st dir = do
    (PlayState b1 s1 gs1 g1) <- readIORef psR
    let updated = updateBoard dir b1
        onSuccessUpdate (BoardUpdated b2 newS2) = do
            -- if we are still alive, this insertion is always possible,
            -- because a successful update means at least two cells
            -- are merged together, leaving at least one empty cell
            (Just b3,g2) <- runRandT (insertNewCell b2) g1
            -- create new PlayState, collect score, sync GameState
            let ps2 = PlayState b3 (s1 + newS2) (gameState b3) g2
            renderGame ps2 items st
            -- update PlayState to the IORef
            writeIORef psR ps2
            return True
        onAlive =
            maybe
                -- 2(a). update failed, invalid move, do nothing
                (return True)
                -- 2(b). updated successfully
                onSuccessUpdate
                -- 2. try to update the board
                updated

    -- 1. only update if alive
    case gs1 of
      Win ->
          return True
      Lose ->
          return True
      Alive ->
          onAlive
      WinAlive ->
          onAlive

-- | the entry for vty-ui CLI implementation
mainVty :: IO ()
mainVty = do
    let cellSample :: String
        cellSample = " 2048 "
        cellLen = length cellSample
        -- spec for a single cell
        -- e.g.:
        cellSpec = ColumnSpec (ColFixed cellLen)
                              (Just AlignRight)
                              (Just (padRight 1))
        helpString = "'i'/'k'/'j'/'l'/arrow keys to move, 'q' to quit."

    -- build up UI
    tbl <- newTable (replicate 4 cellSpec) BorderFull

    pScore <- plainText " <Score> "
    pHelp  <- plainText helpString
    hints  <- hCentered pScore <--> hCentered pHelp
    allW   <- hCentered tbl <--> return hints
    ui     <- centered allW

    fg <- newFocusGroup
    _  <- addToFocusGroup fg pScore

    -- the argument to plainText cannot be an empty string
    -- otherwise the output would be weird
    items  <- (replicateM 4 . replicateM 4) (plainText " ")

    mapM_ (addRow tbl . foldMap mkRow) items

    -- prepare data and initialize
    g <- newStdGen
    ((bd,s),g') <- runRandT initGameBoard g

    -- keep track of playing state using IORef
    let ps = PlayState bd s Alive g'
    playStateR <- newIORef ps

    -- first rendering
    renderGame ps items pScore

    c <- newCollection
    _ <- addToCollection c ui fg

    -- shorthand for event update
    let doUpdate = newDirGameUpdate playStateR items pScore

    pScore `onKeyPressed` \_ key _ ->
        case key of
          KASCII 'q' ->
              shutdownUi >> return True
          KASCII 'i' ->
              doUpdate DUp
          KUp ->
              doUpdate DUp
          KASCII 'k' ->
              doUpdate DDown
          KDown ->
              doUpdate DDown
          KASCII 'j' ->
              doUpdate DLeft
          KLeft ->
              doUpdate DLeft
          KASCII 'l' ->
              doUpdate DRight
          KRight ->
              doUpdate DRight
          _ ->
              return False

    runUi c defaultContext
