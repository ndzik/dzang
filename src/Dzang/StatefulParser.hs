{-# LANGUAGE LambdaCase #-}
module Dzang.StatefulParser where

import           Control.Applicative
import           Control.Monad.State.Lazy
import           Data.Char
import           Data.Functor                   ( (<&>) )
import qualified Dzang.Parser                  as P

data ParserState = ParserState
  { _line :: Int
  , _col  :: Int
  }
  deriving Show

-- Although lifting our existing `P.Parser` into the `StateT` transformer
-- allows us to track external state, all the functions would have to be
-- rewritten or adapted to allow for dependency injection. The culprit here is
-- `item`, which is the only function that really updates the state and is used
-- for every combinator behind the curtains.

type Parser a = StateT ParserState P.Parser a

runParser :: Parser a -> String -> [P.ParserResult (a, ParserState)]
runParser p s = let parser = runStateT p (ParserState 1 1) in P.parse parser s

incCol :: ParserState -> ParserState
incCol s = s { _col = _col s + 1 }

incLine :: ParserState -> ParserState
incLine s = s { _line = _line s + 1 }

item :: Parser Char
item = lift P.item >>= \case
  '\n' -> modify incLine >> return '\n'
  c    -> modify incCol >> return c

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then return c else empty

number :: Parser Integer
number = many (satisfy isDigit) <&> read

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = many space
