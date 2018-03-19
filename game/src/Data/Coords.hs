module Data.Coords where

import Prelude

data Coords = Coords
  { x :: Int
  , y :: Int
  }
  deriving (Show, Eq, Ord)

coords :: Int -> Int -> Coords
coords a b = Coords { x = a, y = b }

prettyPrintCoords :: Coords -> String
prettyPrintCoords (Coords a b)  = "(" ++ show a ++ ", " ++ show b ++ ")"
