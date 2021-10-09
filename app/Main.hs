module Main where

import           Control.Applicative
import           Control.Monad
import qualified Dzang.Parser                  as P
import           Dzang.StatefulParser

newtype Number = Number Integer
  deriving Show

main :: IO ()
main = forever $ do
  l <- getLine
  print $ runParser (spaces *> number) l
