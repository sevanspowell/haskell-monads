module Data.GameEnvironment where

type PlayerName = String

data GameEnvironment = GameEnvironment
  { playerName :: PlayerName
  , debugMode  :: Bool
  }

gameEnvironment :: PlayerName -> Bool -> GameEnvironment
gameEnvironment pName debug = GameEnvironment
  { playerName = pName 
  , debugMode  = debug 
  }
