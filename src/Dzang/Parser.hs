{-# LANGUAGE LambdaCase #-}
module Dzang.Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor                 ( first )
import           Data.Char
import           Data.Functor                   ( (<&>) )

newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- runParser runs the given parser on the given `String` stream and returns the
-- result, effectively unwrapping whatever is contained in it.
runParser :: Parser a -> String -> a
runParser m s = case parse m s of
  [(res, [] )] -> res
  [(_  , _rs)] -> error "Parser did not consume entire stream"
  _            -> error "Unexpected parser error"

-- char parses a single `Char` from the stream.
char :: Parser Char
char = Parser $ \case
  []        -> []
  (c : trs) -> [(c, trs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> join . map (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser $ \s -> [(a, s)]

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> map (first f) . p $ s

instance Applicative Parser where
  pure = unit
--  (Parser p) <*> (Parser q) =
--    Parser $ \s -> [ (f a, s2) | (f, s1) <- p s, (a, s2) <- q s1 ]
  p <*> q = Parser $ parse p >=> (\(f, s1) -> parse q s1 <&> first f)

instance Monad Parser where
  p >>= q = p `bind` q

failure :: Parser a
failure = Parser $ const []

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser $ \s -> parse p s ++ parse q s

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s -> case parse p s of
  [] -> parse q s -- first parse failed, use next option.
  r  -> r

instance Alternative Parser where
  empty = mzero
  (<|>) = option

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = char >>= \c -> if p c then return c else mzero

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

letter :: Char -> Parser Char
letter c = satisfy (c ==)

string :: String -> Parser String
string []       = return []
string (c : cs) = letter c >> string cs >> return (c : cs)

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"
