module Data.GameState where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.Coords (Coords(..), coords)
import Data.GameItem (GameItem(..))

data GameState = GameState
  { items     :: M.Map Coords (S.Set GameItem)
  , player    :: Coords
  , inventory :: S.Set GameItem
  } deriving Show

initialGameState :: GameState
initialGameState = GameState
  { items     = M.fromList [ ( (coords 0 1), (S.singleton Candle)  )
                           , ( (coords 0 0), (S.singleton Matches) )
                           ]
  , player    = Coords { x = 0, y = 0 }
  , inventory = S.empty
  }
