module Main where

import           Control.Applicative          (many, some, (<|>))
import           Control.Monad                (guard)
import           Control.Monad.Except         (catchError, throwError)
import           Control.Monad.Reader         (Reader, ReaderT, ask, local,
                                               runReader, runReaderT)
import           Control.Monad.State          (State, StateT, evalState,
                                               execState, get, modify, put,
                                               runState, runStateT)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Except   (ExceptT, runExceptT)
import           Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import           Control.Monad.Writer         (Writer, WriterT, execWriter,
                                               runWriter, runWriterT, tell)
import           Data.Char                    (toLower, toUpper)
import           Data.Foldable                (traverse_)
import           Data.Functor.Identity
import           Data.List                    (drop, intercalate, stripPrefix,
                                               take)
import           Data.Monoid                  (Sum (..))
import           Data.Word                    (Word)

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

split :: StateT String (Either String) String
split = do
  s <- get
  case s of
    "" -> lift $ Left "Empty string"
    _  -> do
      put (drop 1 s)
      pure (take 1 s)

writerAndExceptT :: ExceptT String (Writer [String]) String
writerAndExceptT = do
  lift $ tell ["Before the error"]
  throwError "Error!"
  lift $ tell ["After the error"]
  pure "Return value"

data Errors = EmptyString
  deriving Show

type Log = [String]

type Parser = StateT String (ExceptT [Errors] (WriterT Log Identity))

split' :: Parser String
split' = do
  s <- get
  tell ["The state is " ++ show s]
  case s of
    "" -> throwError [EmptyString]
    _  -> do
      put (drop 1 s)
      pure (take 1 s)

runParser :: Parser a -> String -> (Either [Errors] (a, String), Log)
runParser p s = runIdentity $ runWriterT $ runExceptT $ runStateT p s

safeDivide :: (Fractional a, Eq a) => a -> a -> ExceptT String Identity a
safeDivide _ 0 = throwError "Denominator is zero"
safeDivide a b = pure (a / b)

type Parser' = StateT String (WriterT Log (ExceptT String Identity))

runParser' :: Parser' a -> String -> Either String ((a, String), Log)
runParser' p s = runIdentity $ runExceptT $ runWriterT $ runStateT p s

string :: String -> Parser' String
string prefix = do
  str <- get
  lift $ tell ["The state is " ++ show str]
  let maybeNoPrefix = stripPrefix prefix str
  case maybeNoPrefix of
    Nothing         -> lift $ lift $ throwError ("Prefix '" ++ prefix ++ "' not present.")
    (Just noPrefix) -> do
      put noPrefix
      pure prefix

type Level' = Int

type Doc' = ReaderT Level (WriterT [String] Identity) ()

renderDoc' :: Doc' -> String
renderDoc' doc = cat' (snd . runIdentity $ runWriterT $ runReaderT doc 0)
  where
    cat' :: [String] -> String
    cat' = intercalate "\n"

line' :: String -> Doc'
line' xs = do
  level <- ask
  let lne = render level xs
  lift $ tell [lne]
  where
    render :: Level -> String -> String
    render 0 str = str
    render l str = render (l - 1) (" " ++ str)

indent' :: Doc' -> Doc'
indent' = local (+ 1)

testDoc :: Doc'
testDoc = do
  line' "Here is some indented text: "
  indent' $ do
    line' "I am indented"
    line' "So am I"
    indent' $ do
      line' "I am even more indented"

testReader' :: String
testReader' = renderDoc' testDoc

upper :: Parser String
upper = do
  s <- split'
  guard $ (toUpper <$> s) == s
  pure s

lower :: Parser String
lower = do
  s <- split'
  guard $ (toLower <$> s) == s
  pure s

upperOrLower :: Parser [String]
upperOrLower = some upper <|> some lower

components :: Parser [[String]]
components = many upperOrLower

main :: IO ()
main = print $ execState test 0
