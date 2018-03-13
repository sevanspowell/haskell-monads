module Main where

import           Control.Monad.Reader  (Reader, ask, local, runReader)
import           Control.Monad.State   (State, StateT, evalState, execState,
                                        get, modify, put, runState)
import           Data.Foldable         (traverse_)
import           Data.Functor.Identity
import           Data.List             (intercalate)

test :: State Int Int
test = do
  put 3
  modify (+1)
  get

sumArray :: [Int] -> State Int ()
sumArray = traverse_ (\n -> modify (\sum' -> sum' + n))

testSumArray :: IO ()
testSumArray = print $ runState (do
                 sumArray [1, 2, 3]
                 sumArray [4, 5]
                 sumArray [6]) 0

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

main :: IO ()
main = print $ execState test 0
