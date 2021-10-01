{-# LANGUAGE LambdaCase #-}
module Dzang.Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor                 ( first )
import           Data.Char
import           Data.Functor                   ( (<&>) )

-- TODO: Extending the parser by processing something like a `ParseResult`
-- which can encode failure states to backpropagate the parsing is advisable.

-- A `Parser` is a function from `String`s to things `a` and `String`s.
newtype Parser a = Parser { parse :: String -> [(a,String)] }

-- runParser runs the given parser on the given input and returns the result.
runParser :: Parser a -> String -> a
runParser p s = case parse p s of
  [(a, [] )] -> a
  [(_, _rs)] -> error "cannot consume whole stream"
  _          -> error "parse error"

instance Functor Parser where
  -- Parse with `Parser` p and map `f` over the results. Remember that a parse
  -- operation might have multiple outcomes, which means we have to map over
  -- every possible outcome.
  fmap f p = Parser $ \s -> map (first f) $ parse p s

instance Applicative Parser where
  pure v = Parser $ \s -> [(v, s)]
  (Parser p) <*> (Parser q) = Parser (p >=> \(f, s) -> map (first f) $ q s)

instance Monad Parser where
  -- Binding a `Parser` to another `Parser` composes the second parse operation
  -- over the result of the first parse operation, yielding a new `Parser`.
  -- Alternative: Parser $ \s -> [ res | (a, s') <- p s, res <- parse (f a) s' ]
  (Parser p) >>= f = Parser (p >=> \(a, s) -> parse (f a) s)

-- With this foundation set, we can start defining some useful `Parser`s.

-- char parses a single char from the stream.
char :: Parser Char
char = Parser $ \case
  []       -> []
  (c : rs) -> [(c, rs)]

instance Alternative Parser where
  empty = Parser $ const []
  p <|> q = Parser $ \s -> case parse p s of
    []  -> parse q s
    res -> res

-- number parses a consecutive amount of digits and returns them as an
-- `Integer`
--  >>> Implementing the `Alternative` typeclass for our `Parser` allows us to
--  use `many`, `some` and `<|>`, which is perfect for this use case.
number :: Parser Integer
number = many (satisfy isDigit) <&> read

space :: Parser Char
space = satisfy isSpace

-- spaces skips all upcoming spaces.
spaces :: Parser String
spaces = many space

-- satisfy uses the input predicate and returns the expected token when
-- encountered.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = char >>= \c -> if p c then return c else empty

separator :: Char -> Parser Char
separator c = satisfy (c ==)

optional :: Parser Char -> Parser Char
optional p = Parser $ \s -> case parse p s of
  []  -> [('x', s)]
  res -> res
