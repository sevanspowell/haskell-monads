module Main where

import Control.Monad.RWS.Lazy (runRWS, liftIO)
import Data.List.Split (splitOn)
import Data.Foldable (for_)

import System.Console.Haskeline

import Data.GameEnvironment
import Data.GameState
import Game

defaultEnv :: GameEnvironment
defaultEnv = GameEnvironment "Player" False

runGame :: GameEnvironment -> GameState -> IO ()
runGame env state = runInputT defaultSettings go
  where
    go :: InputT IO ()
    go = do
        minput <- getInputLine "> "
        case minput of
            Nothing    -> pure ()
            Just input -> liftIO (loop state input)

    loop :: GameState -> String -> IO ()
    loop currentState input = do
        case runRWS (game (splitOn " " input)) env currentState of
            (_, newState, written) -> do
                for_ written putStrLn
                runGame env newState

main :: IO ()
main = runGame defaultEnv initialGameState
