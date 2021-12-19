{-# LANGUAGE LambdaCase #-}
module Dzang.Language where

import           Control.Applicative            ( (<|>) )
import           Data.List
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
                  -- Primitives
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                deriving (Eq)
data Lit = LitInt Integer
         | LitBool Bool
         deriving (Show, Eq)
data Operator = AddOp | SubOp | MulOp | DivOp deriving Show

type OpStack = [Char]
type OprandStack = [Expression]

parseDzang :: String -> Expression
parseDzang = runParser $ parseExpr Env { operators = [], operands = [] }

debugDzang :: String -> [ParserResult Expression]
debugDzang = debugParser $ parseExpr Env { operators = [], operands = [] }

viewVars :: Expression -> [Name]
viewVars (Lambda n e) = n : viewVars e
viewVars _            = []

viewBody :: Expression -> Expression
viewBody (Lambda _ e) = viewBody e
viewBody x            = x

instance Show Expression where
  show (Application expr1 expr2) =
    "[" <> show expr1 <> " " <> show expr2 <> "]"
  show l@(Lambda _ expr) =
    "(λ" <> unwords (viewVars l) <> "." <> show (viewBody expr) <> ")"
  show (Variable n            ) = n
  show (Literal  (LitInt  v)  ) = show v
  show (Literal  (LitBool v)  ) = show v
  show (Module     n     m    ) = show $ "module " <> n <> " where\n" <> show m
  show (Definition n     expr ) = show $ n <> " = " <> show expr
  show (Add        expr1 expr2) = "(" <> show expr1 <> "+" <> show expr2 <> ")"
  show (Sub        expr1 expr2) = "(" <> show expr1 <> "-" <> show expr2 <> ")"
  show (Mul        expr1 expr2) = "(" <> show expr1 <> "*" <> show expr2 <> ")"
  show (Div        expr1 expr2) = "(" <> show expr1 <> "/" <> show expr2 <> ")"

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

parseDef :: Env -> Parser Expression
parseDef env =
  Definition
    <$> name
    <*> (  (optional spaces *> parseBindDef <* optional spaces)
        *> (parseLambda env <|> parseLiteral)
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

-- infixOps is a map of infix operators together with their precendence.
infixOps :: [(Char, Int)]
infixOps = [('+', 0), ('-', 0), ('/', 1), ('*', 1)]

precedence :: Char -> Int
precedence c = case find (\(op, _) -> c == op) infixOps of
  Just (_, prec) -> prec
  Nothing        -> error "unknown operator lookup"

isInfixOp :: Char -> Parser Bool
isInfixOp c = case find (\(op, _) -> c == op) infixOps of
  Nothing -> return False
  _       -> return True

data Env = Env
  { operators :: OpStack
  , operands  :: OprandStack
  }

parseExpr :: Env -> Parser Expression
parseExpr env = parseLVal env >>= parseRVal env

parseLVal :: Env -> Parser Expression
parseLVal env =
  parseLambda env <|> parseLiteral <|> parseVariable <|> parserFail
    "expected either Lambda, Literal, Variable or Application"

-- Conditional on `lval` being a lambda expression -> possible lambda
-- application following.
parseRVal :: Env -> Expression -> Parser Expression
parseRVal env lambda@(Lambda _ _) = peek >>= \case
  Nothing -> return lambda
  _       -> parseApp env lambda
parseRVal env lval = peek >>= \case
  Nothing -> return lval
  (Just c) ->
    isInfixOp c
      >>= (\case
            True  -> parseOperator env lval
            False -> return lval
          )

parseLambda :: Env -> Parser Expression
parseLambda env =
  Lambda
    <$> (optional spaces *> expect 'λ' *> optional spaces *> name)
    <*> (optional spaces *> expect '.' *> optional spaces *> parseExpr env)

parseApp :: Env -> Expression -> Parser Expression
parseApp env lambda = Application lambda <$> (optional spaces *> parseExpr env)

parseVariable :: Parser Expression
parseVariable = Variable <$> name

parseOperator :: Env -> Expression -> Parser Expression
parseOperator env lval = go (operators env) (lval : operands env)
 where
  go :: OpStack -> OprandStack -> Parser Expression
  go os ds@(_ : dd) = peek >>= \case
    Nothing -> return $ mkTree os ds
    Just op -> if precedence op < curPrec os
      then return $ mkTree os ds
      else
        item
        >>  parseExpr env { operators = op : operators env, operands = ds }
        >>= \rval -> peek >>= \case -- abort stream consumption when no new operator is encountered!
              Nothing -> return $ mkTree (op : os) (rval : ds)
              Just c  -> isInfixOp c >>= \case
                True  -> go os (rval : dd)
                False -> return $ mkTree (op : os) (rval : ds)
  go _ _ = parserFail "malformed input"

-- Example: 1*2+3
-- parseExpr [] [] 1*2+3 -> (Literal 1)
--  parseOperator [] [] (Literal 1) *2+3
--    go [] [Literal 1] *2+3
--      parseExpr ['*'] [Literal 1] 2+3 -> Literal 2
--        parseOperator ['*'] [Literal 1] +3
--          go ['*'] [Literal 2, Literal 1] +3
--            mkTree ['*'] [Literal 2, Literal 1] -> Mul 1 2
--          Mul 1 2
--        Mul 1 2
--      go [] [Mul 1 2] +3
--        parseExpr ['+'] [Mul 1 2] -> Literal 3
--        mkTree ['+'] [Literal 3, Mul 1 2] -> Add (Mul 1 2) 3
--      Add (Mul 1 2) 3
--    Add (Mul 1 2) 3
--  Add (Mul 1 2) 3
-- Add (Mul 1 2) 3

mkTree :: OpStack -> OprandStack -> Expression
mkTree ('+' : _) (rhs : lhs : _) = Add lhs rhs
mkTree ('-' : _) (rhs : lhs : _) = Sub lhs rhs
mkTree ('*' : _) (rhs : lhs : _) = Mul lhs rhs
mkTree ('/' : _) (rhs : lhs : _) = Div lhs rhs
mkTree []        _               = error "no operator to make tree"
mkTree _         _               = error "unsupported operator"

curPrec :: OpStack -> Int
curPrec []       = -1
curPrec (op : _) = precedence op
