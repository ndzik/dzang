{-# LANGUAGE LambdaCase #-}
module Dzang.Language where

import           Control.Applicative            ( (<|>) )
import           Data.List
import           Data.Functor                   ( ($>) )
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

data Env = Env
  { operators       :: OpStack
  , operands        :: OprandStack
  , parsingFun      :: Bool
  }
type OpStack = [Char]
type OprandStack = [Expression]

parseDzang :: String -> Expression
parseDzang = runParser $ parseExpr emptyEnv

debugDzang :: String -> [ParserResult Expression]
debugDzang = debugParser $ parseExpr emptyEnv

emptyEnv :: Env
emptyEnv = Env { operators  = []
               , operands   = []
               , parsingFun = False
               }

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
  "module" -> parseModuleKey
  _ -> parserFail "expecting module to start with module header declaration"
 where
  eval' :: Parser [(Name, Expression)]
  eval' = return []
  parseModuleKey :: Parser Expression
  parseModuleKey =
    Module <$> (optional spaces *> name) <*> (optional spaces *> parseWhere *> eval')
  parseWhere :: Parser [(Name, Expression)]
  parseWhere = name >>= \case
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

data Fixity = Infix | Prefix | Postfix deriving Show
data Associativity = LeftAssoc | RightAssoc deriving Show

-- operatorsMap is a map of infix operators together with their precendence.
operatorsMap :: [(Char, Int, Associativity, Fixity)]
operatorsMap =
  [ ('+', 0 , LeftAssoc , Infix)
  , ('-', 0 , LeftAssoc , Infix)
  , ('/', 1 , LeftAssoc , Infix)
  , ('*', 1 , LeftAssoc , Infix)
  , (' ', 20, LeftAssoc , Infix)
  ]

precedence :: Char -> Int
precedence c = case find (\(op, _, _) -> c == op) infixOps of
  Just (_, prec, _) -> prec
  Nothing           -> error "unknown operator lookup"

isInfixOp :: Char -> Bool
isInfixOp c = case find (\(op, _, _) -> c == op) infixOps of
  Nothing -> False
  _       -> True

isInfixOpM :: Char -> Parser Bool
isInfixOpM c = case find (\(op, _, _) -> c == op) infixOps of
  Nothing -> return False
  _       -> return True

isOperator :: Char -> Bool
isOperator c = case find (\(op, _, _, _) -> c == op) operatorsMap of
  Just _ -> True
  _      -> False

isLeftAssoc :: Char -> Bool
isLeftAssoc c = case find (\(op, _, _) -> c == op) infixOps of
  Nothing            -> error "unknown operator lookup"
  Just (_, _, assoc) -> assoc

isRightAssoc :: Char -> Bool
isRightAssoc c = case find (\(op, _, _, _) -> c == op) operatorsMap of
  Just (_, _, RightAssoc, _) -> True
  _                         -> False

parseExpr :: Env -> Parser Expression
parseExpr env@(Env _ ds pf) = parseOperand env >>= \res -> peek >>= \case
  Nothing -> return . head . operands $ printAll env{ operands = res:ds } -- EOF
  Just c
    | (c == ' ' && pf) || c == ')'
                   -> return . head . operands $ printAll env{ operands = res:ds }
    | isOperator c -> parseOperator c env{ operands = res : ds }
    | otherwise    -> parserFail "unexpected continuation of stream"

-- Shunting-Yard-Algorithm:
-- http://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/
parseOperator :: Char -> Env -> Parser Expression
parseOperator cop env@(Env [] _ _) = item >> parseExpr env{ operators = [cop] }
parseOperator cop env@(Env os _ _)
     | (curPrec os < precedence cop)
       || (curPrec os == precedence cop
           && isRightAssoc cop)  = item >> parseExpr env{ operators = cop:os }
     | (curPrec os > precedence cop)
       || (curPrec os == precedence cop
           && isLeftAssoc cop)   = parseOperator cop $ popAndPrint env
parseOperator _ _                = error "unhandled case in parseOperator"

popAndPrint :: Env -> Env
popAndPrint (Env [] [] _)                          = error "should not happen"
popAndPrint env@(Env [] [_] _)                     = env
popAndPrint env@(Env ('+': os) (rhs : lhs : ds) _) = env{ operators = os, operands = Add lhs rhs : ds }
popAndPrint env@(Env ('-': os) (rhs : lhs : ds) _) = env{ operators = os, operands = Sub lhs rhs : ds }
popAndPrint env@(Env ('/': os) (rhs : lhs : ds) _) = env{ operators = os, operands = Div lhs rhs : ds }
popAndPrint env@(Env ('*': os) (rhs : lhs : ds) _) = env{ operators = os, operands = Mul lhs rhs : ds }
popAndPrint env@(Env (' ': os) (rhs : lhs : ds) _) = env{ operators = os, operands = Application lhs rhs : ds }
popAndPrint (Env os ds _)                          = error $ printf "unhandled case: %n %n" (length os) (length ds)

printAll :: Env -> Env
printAll env@(Env [] [_] _) = env
printAll env = printAll . popAndPrint $ env

parseOperand :: Env -> Parser Expression
parseOperand _ = parseBracket emptyEnv
               <|> parseLiteral
               <|> parseLambda emptyEnv
               <|> parseVariable

parseBracket :: Env -> Parser Expression
parseBracket env = peek >>= \case
  Just '('  -> item >> parseExpr env >>= \body -> expect ')' $> body
  _         -> parserFail "expected opening bracket"

parseLambda :: Env -> Parser Expression
parseLambda env = Lambda
      <$> (expect 'λ' *> optional spaces *> name)
      <*> (optional spaces *> expect '.' *> optional spaces *> parseExpr env{ parsingFun = True })

parseApp :: Env -> Expression -> Parser Expression
parseApp env expr = Application expr <$> (optional spaces *> parseExpr env)

parseVariable :: Parser Expression
parseVariable = Variable <$> name

curPrec :: OpStack -> Int
curPrec []       = -1
curPrec (op : _) = precedence op

curAssocLeft :: OpStack -> Bool
curAssocLeft [] = False
curAssocLeft (op:_) = isLeftAssoc op
