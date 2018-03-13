module Main where

import           Control.Monad.State   ( State
                                       , StateT
                                       , evalState
                                       , execState
                                       , runState
                                       , get
                                       , modify
                                       , put
                                       )
import           Data.Foldable         (traverse_)
import           Data.Functor.Identity

test :: State Int Int
test = do
  put 3
  modify (+1)
  get

sumArray :: [Int] -> State Int ()
sumArray = traverse_ (\n -> modify (\sum -> sum + n))

testSumArray :: IO ()
testSumArray = print $ runState (do
                 sumArray [1, 2, 3]
                 sumArray [4, 5]
                 sumArray [6]) 0

main :: IO ()
main = print $ execState test 0
