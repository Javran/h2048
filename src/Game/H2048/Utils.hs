{-|
  Module      : Game.H2048.Utils
  Copyright   : (c) 2014 Javran Cheng
  License     : MIT
  Maintainer  : Javran.C@gmail.com
  Stability   : experimental
  Portability : POSIX

helper functions used when implementing game logic

-}
module Game.H2048.Utils
  ( inPos
  )
where

import Control.Lens

-- | modify a specified element in a list,
--   this is a simple semantic editor combinator
inPos :: Int      -- ^ the index
      -> (a -> a) -- ^ a function from the old element to the new one
      -> [a]      -- ^ the list to be modified
      -> [a]
inPos n = over (ix n)
