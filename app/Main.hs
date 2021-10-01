module Main where

import           Control.Monad
import           Dzang.Parser

data Number = Number Integer Integer
  deriving Show

main :: IO ()
main = forever $ do
  l <- getLine
  print $ runParser p l
 where
  p = spaces *> res <* spaces
  res :: Parser Number
  res =
    mkNumber
      <*> number
      <*> (spaces >> optional (separator ',') >> spaces >> number)
  mkNumber :: Parser (Integer -> Integer -> Number)
  mkNumber = return Number
