{-# LANGUAGE LambdaCase #-}
module Dzang.Language where

import           Control.Applicative            ( (<|>) )
import           Dzarser.Parser
import           Text.Printf

-- Dzang is a simply typed lambda calculus language with typeinference.

type Name = String
                  -- (λa.12+a) 42 -> λ42.12+42 -> λ42.54 -> 54
data Expression = Application Expression Expression
                  -- (λa.12+a)
                | Lambda Name Expression
                | Variable Name
                | Literal Lit
                | Definition Name Expression
                  -- Do I want it to be possible to bind `Module`s to variables?
                | Module Name [(Name, Expression)]
                deriving (Eq)
data Lit = LitInt Integer
         | LitBool Bool
         deriving (Show, Eq)

viewVars :: Expression -> [Name]
viewVars (Lambda n e) = n : viewVars e
viewVars _            = []

viewBody :: Expression -> Expression
viewBody (Lambda _ e) = viewBody e
viewBody x            = x

instance Show Expression where
  show (Application expr1 expr2) = show expr1 <> " " <> show expr2
  show l@(Lambda _ expr) =
    "λ" <> unwords (viewVars l) <> "." <> show (viewBody expr)
  show (Variable n          ) = n
  show (Literal  (LitInt  v)) = show v
  show (Literal  (LitBool v)) = show v
  show (Module     n m      ) = show $ "module " <> n <> " where\n" <> show m
  show (Definition n expr   ) = show $ n <> " = " <> show expr

reserved :: [Name]
reserved = ["module", "where"]

parseModule :: Parser Expression
parseModule = name >>= \case
  "module" -> modul
  _ -> parserFail "expecting module to start with module header declaration"
 where
  eval' :: Parser [(Name, Expression)]
  eval' = return []
  modul :: Parser Expression
  modul =
    Module <$> (optional spaces *> name) <*> (optional spaces *> wher *> eval')
  wher :: Parser [(Name, Expression)]
  wher = name >>= \case
    "where" -> eval'
    res     -> parserFail
      $ printf "expecting 'where' to close module definition got: %s" res

parseDef :: Parser Expression
parseDef =
  Definition
    <$> name
    <*> (  (optional spaces *> parseBindDef <* optional spaces)
        *> (parseLambda <|> parseLiteral)
        )

parseBindDef :: Parser Char
parseBindDef = satisfy (== '=')
  $ printf "expecting the start of a binding definition with '='"

parseLiteral :: Parser Expression
parseLiteral =
  Literal
    <$> (parseInt <|> parseBool <|> parserFail
          "expecting literal of either Bool or Int"
        )

parseInt :: Parser Lit
parseInt = LitInt <$> number

parseBool :: Parser Lit
parseBool =
  LitBool
    <$> (   name
        >>= (\case
              "true"  -> return True
              "false" -> return False
              a ->
                parserFail $ printf "expected 'true' or 'false' but got: %s" a
            )
        )

parseExpr :: Parser Expression
parseExpr =
  parseLambda <|> parseLiteral <|> parseVariable <|> parseApp <|> parserFail
    "expected either Lambda, Literal, Variable or Application"


parseLambda :: Parser Expression
parseLambda =
  Lambda
    <$> (optional spaces *> expect 'λ' *> optional spaces *> name)
    <*> (optional spaces *> expect '.' *> optional spaces *> parseExpr)

parseApp :: Parser Expression
parseApp = undefined

parseVariable :: Parser Expression
parseVariable = Variable <$> name
