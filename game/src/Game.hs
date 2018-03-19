module Game where

import Data.GameEnvironment
import Data.GameState
import Data.GameItem

import Data.Map as M
import Data.Set as S
import Control.Monad.RWS.Lazy

type Log = [String]

type Game = RWS GameEnvironment Log GameState

has :: GameItem -> Game Bool
has item = do
  s <- get
  pure $ item `S.member` (inventory s)

pickUp :: GameItem -> Game ()
pickUp item = do
  s <- get
  case (player s) `M.lookup` (items s) of
    Just itemsHere
      | item `S.member` itemsHere -> do
          let newItems = M.update (Just . S.delete item) (player s) (items s)
              newInventory = S.insert item (inventory s)
          put $ GameState { items     = newItems
                          , inventory = newInventory
                          , player    = (player s)
                          }
          tell (["You now have the " ++ show item])
    _ -> tell (["I don't see that item here."])
