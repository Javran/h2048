module Game.H2048.Utils
    ( inPos
    , universe
    )
where

-- provide utilities

-- | all possible values for a Bounded Enum
universe :: (Bounded e, Enum e) => [e]
universe = [minBound .. maxBound]

-- | modify a specified element in a list,
--   this is a simple semantic editor combinator
inPos :: Int      -- ^ the index
      -> (a -> a) -- ^ a function from the old element to the new one
      -> [a]      -- ^ the list to be modified
      -> [a]
inPos n f xs
      | null xs    = xs
      | n < 0      = xs
      -- for all the cases below,
      -- safely assume n >= 0 and xs is not empty
      | n == 0     = let (y:ys) = xs
                     in f y : ys
      | otherwise  = let (y:ys) = xs
                     in y : inPos (n - 1) f ys
