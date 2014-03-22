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
-- import Control.Concurrent
-- import Data.Monoid
import Data.Foldable (foldMap)

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
    (mapM_ . mapM_) (`setTextWithAttrs` [(" ", Attr Default (SetTo cyan) Default )]) items

    -- some random cells for visualization
    mapM_ (\(ind,x) ->
               setTextWithAttrs x [( T.pack ( show (2 ^ (1+( ind * 37 `mod` 7) ) :: Int))
                                   , Attr (SetTo bold) (SetTo (ISOColor ind)) Default)])
         $ zip (filter (/=8) [1..]) $ concat items

    c <- newCollection
    _ <- addToCollection c ui fg

    pScore `onKeyPressed` \_ key _ ->
        if key == KASCII 'q'
            then shutdownUi >> return True
            else return False

    runUi c defaultContext
