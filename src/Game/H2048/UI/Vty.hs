{-# LANGUAGE OverloadedStrings #-}
module Game.H2048.UI.Vty
    ( mainVty
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

data PlayState g = PlayState Board Int g

-- pair: ({val}, ({row},{col}))
toIndexedBoard :: Board -> [(Int, (Int, Int))]
toIndexedBoard b = concat $ zipWith go [0..] taggedCols
    where
        -- board with each cell tagged with its col num
        taggedCols = map (zip [0..]) b
        go :: Int -> [(Int,Int)] -> [(Int,(Int,Int))]
        go row = map (\(col,val) -> (val,(row,col)))

renderGame :: PlayState g -> [[Widget FormattedText]] ->  IO ()
renderGame (PlayState bd _ _) items = do
    let ixBd = toIndexedBoard bd
        renderCell v (row,col) = do
            let item = items !! row !! col
            item `setTextWithAttrs` [ ( (T.pack . show) v, Attr Default (SetTo cyan) Default )   ]

    mapM_ (uncurry renderCell) ixBd

newDirGameUpdate :: (RandomGen g)
                 => IORef (PlayState g)
                 -> [[Widget FormattedText]]
                 ->  Dir
                 -> IO Bool
newDirGameUpdate psR items dir = do
    (PlayState b1 s1 g1) <- readIORef psR
    let updated = updateBoard dir b1
        onSuccessUpdate (BoardUpdated b2 newS2) = do
            -- TODO random.
            (maybeB3,g2) <- runRandT (insertNewCell b2) g1
            let b3 = fromMaybe b2 maybeB3
                ps2 = PlayState b3 (s1 + newS2) g2
            renderGame ps2 items
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
        scoreString :: Int -> T.Text
        scoreString = T.pack . ("Current score: " ++) . show

    -- build up UI
    tbl <- newTable (replicate 4 cellSpec) BorderFull

    pScore <- plainText $ scoreString 23333
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
    let ps = PlayState bd s g'
    playStateR <- newIORef ps

    renderGame ps items

    c <- newCollection
    _ <- addToCollection c ui fg

    pScore `onKeyPressed` \_ key _ ->
        case key of
          KASCII 'q' ->
              shutdownUi >> return True
          KASCII 'i' ->
              newDirGameUpdate playStateR items DUp
          KASCII 'k' ->
              newDirGameUpdate playStateR items DDown
          KASCII 'j' ->
              newDirGameUpdate playStateR items DLeft
          KASCII 'l' ->
              newDirGameUpdate playStateR items DRight
          _ ->
              return False

    runUi c defaultContext
