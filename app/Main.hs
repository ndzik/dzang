module Main where

import           Dzang.Parser
import           Dzang.Syntax

main :: IO ()
main = do
  print (runParser name "abcdefg")
  print (runParser name' "abcdefg")
  print (runParser name'' "abcdefg")
