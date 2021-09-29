module Dzang.Syntax where

import           Control.Applicative
import           Data.Functor                   ( (<&>) )
import           Dzang.Parser

newtype Name = Name String deriving (Show)

newtype Syntax = Var Name

name :: Parser Name
name = do
  s <- many char
  return $ Name s

name' :: Parser Name
name' = many char >>= \s -> return $ Name s

name'' :: Parser Name
name'' = many char <&> Name
