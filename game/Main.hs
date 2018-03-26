module Main where

import Control.Monad
import Control.Monad.RWS.Lazy (runRWS, liftIO)
import Data.List.Split (splitOn)
import Data.Foldable (for_)

import System.Console.Haskeline
import Sound.ALUT

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

-- Chris Double
-- https://bluishcoder.co.nz/articles/haskell/openal.html
playSound :: IO ()
playSound =
  withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ ->
  do
    (Just device) <- openDevice Nothing
    (Just context) <- createContext device []
    currentContext $= Just context
    buffer1 <- createBuffer $ Sine 440 0 1
    buffer2 <- createBuffer HelloWorld
    [source] <- genObjectNames 1
    queueBuffers source [buffer1,buffer2]
    play [source]
    sleep 4
    closeDevice device
    return ()

main :: IO ()
main = runGame defaultEnv initialGameState
-- main = playSound

