module Game where

import Data.GameEnvironment
import Data.GameState
import Data.GameItem
import Data.Coords

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

describeRoom :: Game ()
describeRoom = do
  s <- get
  case (player s) of
    Coords { x = 0, y = 0 } -> tell (["You are in a dark forest. You see a path to the north."])
    Coords { x = 0, y = 1 } -> tell (["You are in a clearing."]) 
    _ -> tell (["You are deep in the forest"])

move :: Int -> Int -> Game ()
move dx dy = modify (\s -> GameState { items = (items s)
                                     , inventory = (inventory s)
                                     , player = updateCoords (player s)
                                     })
  where
  updateCoords :: Coords -> Coords
  updateCoords (Coords px py) = coords (px + dx) (py + dy)

use :: GameItem -> Game ()
use Candle = tell (["I don't know what you want me to do with that."])
use Matches = do
  hasCandle <- has Candle
  if hasCandle
  then do
    env <- ask
    tell (["You light the candle."])
    tell (["Congratiulations, " ++ playerName env ++ "!"])
    tell (["You win!"])
  else
    tell (["You don't have anything to light."])

game :: [String] -> Game ()
game ["look"] = do
  s <- get
  tell (["You are at " ++ prettyPrintCoords (player s)])
  describeRoom
  forM_ (M.lookup (player s) (items s)) $ \items' ->
    tell (fmap (\item -> "You see the " ++ show item ++ ".") (S.toList items'))
game ["inventory"] = do
  s <- get
  tell (fmap (\item -> "You have the " ++ show item ++ ".") (S.toList (inventory s)))
game ["north"] = move 0    (1)
game ["south"] = move 0    (-1)
game ["west"]  = move (-1) 0
game ["east"]  = move 1 0 
game ["take", item] =
  case readItem item of
    Nothing -> tell ["I don't know what item you're referring to."]
    Just gameItem -> pickUp gameItem
game ["use", item] =
  case readItem item of
    Nothing -> tell ["I don't know what item you're referring to."]
    Just gameItem -> do
      hasItem <- has gameItem
      if hasItem
        then use gameItem
        else tell ["You don't have that item"]
game ["debug"] = do
  env <- ask
  if debugMode env
    then do
    s <- get
    tell [show s]
  else tell ["Not running in debug mode."]
game [] = pure ()
game _ = tell ["I don't understand"]

