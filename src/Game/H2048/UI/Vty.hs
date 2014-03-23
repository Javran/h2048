{-# LANGUAGE OverloadedStrings #-}
module Game.H2048.UI.Vty
    ( PlayState (..)
    , mainVty
    )
where

import Graphics.Vty.Widgets.All
import Graphics.Vty
-- import Graphics.Vty.Attributes
-- import Graphics.Vty.LLInput
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Random
-- import Control.Concurrent
-- import Data.Monoid
import Data.Foldable (foldMap)
import Data.IORef
import Data.Maybe

import Game.H2048.Core

data PlayState g = PlayState
    { psBoard  :: Board
    , psScore  :: Int
    , psGState :: GameState
    , psRGen   ::  g
    }

-- pair: ({val}, ({row},{col}))
toIndexedBoard :: Board -> [(Int, (Int, Int))]
toIndexedBoard b = concat $ zipWith go [0..] taggedCols
    where
        -- board with each cell tagged with its col num
        taggedCols = map (zip [0..]) b
        go :: Int -> [(Int,Int)] -> [(Int,(Int,Int))]
        go row = map (\(col,val) -> (val,(row,col)))

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

renderGame :: PlayState g
           -> [[Widget FormattedText]]
           -> Widget FormattedText
           ->  IO ()
renderGame (PlayState bd sc gs _) items st = do
    let ixBd = toIndexedBoard bd
        renderCell v (row,col) = do
            let item = items !! row !! col
            item `setTextWithAttrs` colorize v
        scoreDesc = case gs of
                      Win -> "You win. Final Score: "
                      Lose -> "Game Over. Final Score: "
                      Alive -> "Current Score: "

    mapM_ (uncurry renderCell) ixBd
    setText st $ T.pack (scoreDesc ++ show sc)

newDirGameUpdate :: (RandomGen g)
                 => IORef (PlayState g)
                 -> [[Widget FormattedText]]
                 -> Widget FormattedText
                 ->  Dir
                 -> IO Bool
newDirGameUpdate psR items st dir = do
    (PlayState b1 s1 gs1 g1) <- readIORef psR
    let updated = updateBoard dir b1
        onSuccessUpdate (BoardUpdated b2 newS2) = do
            (maybeB3,g2) <- runRandT (insertNewCell b2) g1
            let b3 = fromMaybe b2 maybeB3
                ps2 = PlayState b3 (s1 + newS2) gs1 g2
            renderGame ps2 items st
            writeIORef psR ps2
            return True
    maybe
        (return True)
        onSuccessUpdate
        updated

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
        helpString = "'i'/'k'/'j'/'l' to move, 'q' to quit."

    -- build up UI
    tbl <- newTable (replicate 4 cellSpec) BorderFull

    pScore <- plainText " <Score> "
    pHelp  <- plainText   helpString
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

    let ps = PlayState bd s Alive  g'
    playStateR <- newIORef ps

    renderGame ps items pScore

    c <- newCollection
    _ <- addToCollection c ui fg

    pScore `onKeyPressed` \_ key _ ->
        case key of
          KASCII 'q' ->
              shutdownUi >> return True
          KASCII 'i' ->
              newDirGameUpdate playStateR items pScore DUp
          KASCII 'k' ->
              newDirGameUpdate playStateR items pScore DDown
          KASCII 'j' ->
              newDirGameUpdate playStateR items pScore DLeft
          KASCII 'l' ->
              newDirGameUpdate playStateR items pScore DRight
          _ ->
              return False

    runUi c defaultContext
