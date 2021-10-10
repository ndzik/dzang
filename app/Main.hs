module Main where

import           Control.Exception
import           Control.Monad
import           Data.Functor
import           Dzang.Parser

data Number = Number Integer Integer
  deriving Show

main :: IO ()
main = forever $ do
  l <- getLine
  res <- try
    ( print
    $ runParser (spaces $> Number <*> (number <* expect ',') <*> number) l
    ) :: IO (Either SomeException ())
  case res of
    Left err -> print err
    _ -> return ()
