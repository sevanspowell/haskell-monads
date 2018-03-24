module Split where

import           Prelude

import           Control.Monad              (guard)
import           Control.Monad.Except       (throwError)
import           Control.Monad.State        (StateT, get, put, runStateT)
import           Control.Monad.Trans.Except (ExceptT, runExcept)
import           Control.Monad.Writer       (WriterT, runWriterT, tell)
import           Data.Char                  (toLower, toUpper)
import           Data.Functor.Identity      (Identity)

type Errors = [String]

type Log = [String]

type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

split :: Parser String
split = do
  s <- get
  tell ["The state is " ++ show s]
  case s of
    "" -> throwError ["Empty string"]
    _  -> do
      put  (drop 1 s)
      pure (take 1 s)

eof :: Parser ()
eof = do
  s <- get
  tell ["The state is " ++ show s]
  case s of
    "" -> pure ()
    _  -> throwError ["Expected end-of-file"]

upper :: Parser String
upper = do
  s <- split
  guard $ fmap toUpper s == s
  pure s

lower :: Parser String
lower = do
  s <- split
  guard $ fmap toLower s == s
  pure s

runParser :: Parser a -> String -> Either Errors ((a, String), Log)
runParser p = runExcept . runWriterT . runStateT p
