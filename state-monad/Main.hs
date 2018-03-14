module Main where

import           Control.Monad.Reader  (Reader, ask, local, runReader)
import           Control.Monad.State   (State, StateT, evalState, execState,
                                        get, modify, put, runState)
import           Control.Monad.Writer  (Writer, tell, runWriter, execWriter)
import           Data.Foldable         (traverse_)
import           Data.Functor.Identity
import           Data.List             (intercalate)
import           Data.Monoid           (Sum(..))
import           Data.Word             (Word)

test :: State Int Int
test = do
  put 3
  modify (+1)
  get

sumArray :: [Int] -> State Int ()
sumArray = traverse_ (\n -> modify (\sum' -> sum' + n))

sumArray' :: [Int] -> Writer (Sum Int) () 
sumArray' = traverse_ (\n -> tell (Sum n))

testSumArray :: IO ()
testSumArray = print $ runState (do
                 sumArray [1, 2, 3]
                 sumArray [4, 5]
                 sumArray [6]) 0

testSumArray' :: IO ()
testSumArray' = print $ runWriter (do
                 sumArray' [1, 2, 3]
                 sumArray' [4, 5]
                 sumArray' [6])

-- | Determine if a string of parentheses is balanced.
-- > testParens ""
-- True
--
-- > testParens "(()(())())"
-- True
--
-- > testParens ")"
-- False
--
-- > testParens "(()()"
-- False
balancedParens :: String -> Bool
balancedParens xs = (execState (openParens xs) 0) == 0
  where
    openParens :: String -> State Int ()
    openParens = traverse_ (\c -> case c of
                                '(' -> modify ((+) 1)
                                ')' -> modify ((-) 1)
                                _   -> modify (id))

type Level = Int

type Doc = Reader Level String

-- | Render string at current indent level
-- > runReader (line "str") 0
-- "str"
-- > runReader (line "str") 2
-- "  str"
-- > runReader (line "str") 3
-- "   str"
-- > runReader (line "str") 3
-- "   str"
line :: String -> Doc
line xs = do
  level <- ask
  pure (render level xs)
  where
    render :: Level -> String -> String
    render 0 str = str
    render l str = render (l - 1) (" " ++ str)

-- | Increases the indentation level for a block of code.
-- > runReader (indent (line "str")) 0
-- " str"
indent :: Doc -> Doc
indent = local (+ 1)

-- | Concatenate a collection of documents
cat :: [Doc] -> Doc
cat ds = do
  xs <- sequence ds
  pure (intercalate "\n" xs)

renderDoc :: Doc -> String
renderDoc doc = runReader doc 0

testReader :: String
testReader = renderDoc $ cat
  [ line "Here is some indented text:"
  , indent $ cat
      [ line "I am indented"
      , line "So am I"
      , indent $ line "I am even more indented"
      ]
  ]

-- gcd :: Int -> Int -> Int
-- gcd n 0 = n
-- gcd 0 m = m
-- gcd n m = if n > m
--             then gcd (n - m) m
--             else gcd n (m - n)

gcdLog :: Int -> Int -> Writer [String] Int
gcdLog n 0 = pure n
gcdLog 0 m = pure m
gcdLog n m = do
  tell ["gcdLog " ++ show n ++ " " ++ show m]
  if n > m
    then gcdLog (n - m) m
    else gcdLog n (m - n)

-- Number of iterations for collatz sequence to reach 1.
collatzNum :: Word -> Word 
collatzNum x = collatz x
  where
    collatz :: Word -> Word
    collatz 1 = 0
    collatz n = case even n of
        True  -> do
          1 + collatz (n `quot` 2)
        False -> do
          1 + collatz (3 * n + 1)

collatzNum' :: Word -> Writer [String] Word 
collatzNum' x = collatz x
  where
    collatz :: Word -> Writer [String] Word
    collatz 1 = do
      tell [show 1]
      pure 0
    collatz n = case even n of
        True  -> do
          tell [show n]
          cnt <- collatz (n `quot` 2)
          pure $ cnt + 1
        False -> do
          tell [show n]
          cnt <- collatz (3 * n + 1)
          pure $ cnt + 1

main :: IO ()
main = print $ execState test 0
