module Data.GameItem where

import Prelude

data GameItem = Candle | Matches
  deriving (Show, Eq, Ord)

readItem :: String -> Maybe GameItem
readItem "Candle"  = Just Candle
readItem "Matches" = Just Matches
readItem _         = Nothing
