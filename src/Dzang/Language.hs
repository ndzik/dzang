{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Dzang.Language where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List
import Dzang.AST
import Dzarser.Base
import Dzarser.Combinator
import Dzarser.Stateful.Parser
import Text.Printf

data Env = Env
  { operators :: OpStack,
    operands :: OprandStack,
    parsingFun :: Bool
  }

type OpStack = [(Char, (Int, Int))]

type OprandStack = [Expression]

type FuncStack = [Expression]

type FuncBodyStack = [Expression]

parseDzang :: String -> (Either ParseError Expression, ParserState)
parseDzang = runParser $ parseExpr emptyEnv

evalDzang :: String -> Either ParseError Expression
evalDzang = fst . parseDzang

emptyEnv :: Env
emptyEnv = Env {operators = [], operands = [], parsingFun = False}

reserved :: [Name]
reserved = ["module", "where"]

parseModule :: Parser Expression
parseModule =
  name >>= \case
    "module" -> parseModuleKey
    _ -> parserFail "expecting module to start with module header declaration"
  where
    eval' :: Parser [(Name, Expression)]
    eval' = return []
    parseModuleKey :: Parser Expression
    parseModuleKey =
      mkModule
        <$> (optional spaces *> name)
        <*> (optional spaces *> parseWhere *> eval')
    parseWhere :: Parser [(Name, Expression)]
    parseWhere =
      name >>= \case
        "where" -> eval'
        res ->
          parserFail $
            printf "expecting 'where' to close module definition got: %s" res

parseDef :: Env -> Parser Expression
parseDef env =
  mkDefinition
    <$> parserPos
    <*> name
    <*> ((optional spaces *> parseBindDef <* optional spaces) *> parseExpr env)

parseBindDef :: Parser Char
parseBindDef =
  satisfy (== '=') $
    printf "expecting the start of a binding definition with '='"

parseLiteral :: Parser Expression
parseLiteral =
  mkLiteral
    <$> parserPos
      <*> ( parseInt <|> parseBool
              <|> parserFail
                "expecting literal of either Bool or Int"
          )

parseInt :: Parser Lit
parseInt = LitInt <$> number

parseBool :: Parser Lit
parseBool =
  LitBool
    <$> ( name
            >>= \case
              "true" -> return True
              "false" -> return False
              a -> parserFail $ printf "expected 'true' or 'false' but got: %s" a
        )

data Fixity = Infix | Prefix | Postfix deriving (Show)

data Associativity = LeftAssoc | RightAssoc deriving (Show)

-- operatorsMap is a map of infix operators together with their precendence.
operatorsMap :: [(Char, Int, Associativity, Fixity)]
operatorsMap =
  [ ('+', 0, LeftAssoc, Infix),
    ('-', 0, LeftAssoc, Infix),
    ('/', 1, LeftAssoc, Infix),
    ('*', 1, LeftAssoc, Infix),
    (' ', 20, LeftAssoc, Infix)
  ]

precedence :: Char -> Int
precedence c = case find (\(op, _, _, _) -> c == op) operatorsMap of
  Just (_, prec, _, _) -> prec
  Nothing -> error "unknown operator lookup"

isInfixOp :: Char -> Bool
isInfixOp c = case find (\(op, _, _, _) -> c == op) operatorsMap of
  Just (_, _, _, Infix) -> True
  _ -> False

isInfixOpM :: Char -> Parser Bool
isInfixOpM c = return $ isInfixOp c

isOperator :: Char -> Bool
isOperator c = case find (\(op, _, _, _) -> c == op) operatorsMap of
  Just _ -> True
  _ -> False

isLeftAssoc :: Char -> Bool
isLeftAssoc c = case find (\(op, _, _, _) -> c == op) operatorsMap of
  Just (_, _, LeftAssoc, _) -> True
  _ -> False

isRightAssoc :: Char -> Bool
isRightAssoc c = case find (\(op, _, _, _) -> c == op) operatorsMap of
  Just (_, _, RightAssoc, _) -> True
  _ -> False

parseExpr :: Env -> Parser Expression
parseExpr env@(Env _ ds pf) =
  parseOperand env >>= \res ->
    peek >>= \case
      Nothing -> return . head . operands $ printAll env {operands = res : ds} -- EOF
      Just c
        | (c == ' ' && pf) || c == ')' ->
          return . head . operands $
            printAll
              env
                { operands = res : ds
                }
        | isOperator c -> parseOperator c env {operands = res : ds}
        | otherwise -> parserFail "unexpected continuation of stream"

-- Shunting-Yard-Algorithm:
-- http://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/
parseOperator :: Char -> Env -> Parser Expression
parseOperator cop env@(Env [] _ _) = do
  pos <- parserPos
  item >> parseExpr env {operators = [(cop, pos)]}
parseOperator cop env@(Env os _ _)
  | (curPrec os < precedence cop)
      || (curPrec os == precedence cop && isRightAssoc cop) = do
    pos <- parserPos
    item >> parseExpr env {operators = (cop, pos) : os}
  | (curPrec os > precedence cop)
      || (curPrec os == precedence cop && isLeftAssoc cop) =
    parseOperator cop $ popAndPrint env
parseOperator _ _ = error "unhandled case in parseOperator"

popAndPrint :: Env -> Env
popAndPrint env@(Env [] [_] _) = env
popAndPrint env@(Env (('+', pos) : os) (rhs : lhs : ds) _) =
  env {operators = os, operands = mkAdd pos lhs rhs : ds}
popAndPrint env@(Env (('-', pos) : os) (rhs : lhs : ds) _) =
  env {operators = os, operands = mkSub pos lhs rhs : ds}
popAndPrint env@(Env (('/', pos) : os) (rhs : lhs : ds) _) =
  env {operators = os, operands = mkDiv pos lhs rhs : ds}
popAndPrint env@(Env (('*', pos) : os) (rhs : lhs : ds) _) =
  env {operators = os, operands = mkMul pos lhs rhs : ds}
popAndPrint env@(Env ((' ', pos) : os) (rhs : lhs : ds) _) =
  env {operators = os, operands = mkApplication pos lhs rhs : ds}
popAndPrint (Env os ds _) =
  error $ printf "unhandled case: %n %n" (length os) (length ds)

printAll :: Env -> Env
printAll env@(Env [] [_] _) = env
printAll env = printAll . popAndPrint $ env

parseOperand :: Env -> Parser Expression
parseOperand env =
  parseBracket emptyEnv
    <|> parseLiteral
    <|> parseLambda emptyEnv
    <|> parseDef env
    <|> parseVariable

parseBracket :: Env -> Parser Expression
parseBracket env =
  peek >>= \case
    Just '(' -> item >> parseExpr env >>= \body -> expect ')' $> body
    _ -> parserFail "expected opening bracket"

parseLambda :: Env -> Parser Expression
parseLambda env =
  mkLambda
    <$> parserPos
    <*> (expect '??' *> optional spaces *> name)
    <*> ( optional spaces *> expect '.' *> optional spaces
            *> parseExpr
              env
                { parsingFun = True
                }
        )

parseApp :: Env -> Expression -> Parser Expression
parseApp env expr = mkApplication <$> parserPos <*> return expr <*> (optional spaces *> parseExpr env)

parseVariable :: Parser Expression
parseVariable = mkVariable <$> parserPos <*> name

curPrec :: OpStack -> Int
curPrec [] = -1
curPrec ((op, _) : _) = precedence op

curAssocLeft :: OpStack -> Bool
curAssocLeft [] = False
curAssocLeft ((op, _) : _) = isLeftAssoc op
