module Main where

import           Control.Exception
import           Control.Monad
import           Dzarser.Parser

main :: IO ()
main = forever $ do
  l   <- getLine
  res <-
    try (print $ runParser (optional space >> number) l) :: IO
      (Either SomeException ())
  case res of
    Left  err -> print err
    Right _ -> return ()
