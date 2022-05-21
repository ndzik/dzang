{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Dzang.AST where

import Data.Fix (Fix (..))
import Data.Functor.Classes (Eq1 (..))
import Data.Functor.Foldable

type Name = String

data Lit
  = LitInt Integer
  | LitBool Bool
  deriving (Show, Eq)

type Expression = Fix ExpressionF

-- (λa.12+a) 42 -> λ42.12+42 -> λ42.54 -> 54
data ExpressionF r
  = Application r r
  | -- (λa.12+r)
    Lambda Name r
  | Variable Name
  | Literal Lit
  | Definition Name r
  | -- Do I want it to be possible to bind `Module`s to variables?
    Module Name [(Name, r)]
  | -- Primitives
    Add r r
  | Sub r r
  | Mul r r
  | Div r r
  deriving (Show, Eq, Functor)

-- Alternatively with {-#LANGUAGE TemplateHaskell#-}:
-- $(deriveEq1 ''ExpressionF)

instance Eq1 ExpressionF where
  liftEq f (Application a1 a2) (Application b1 b2) = f a1 b1 && f a2 b2
  liftEq f (Lambda na a) (Lambda nb b) = na == nb && f a b
  liftEq _ (Variable a) (Variable b) = a == b
  liftEq _ (Literal a) (Literal b) = a == b
  liftEq f (Definition na a) (Definition nb b) = na == nb && f a b
  liftEq f (Module na a) (Module nb b) = na == nb && and (zipWith (curry (\((n, c), (n', c')) -> n == n' && f c c')) a b)
  liftEq f (Add a1 a2) (Add b1 b2) = f a1 b1 && f a2 b2
  liftEq f (Sub a1 a2) (Sub b1 b2) = f a1 b1 && f a2 b2
  liftEq f (Mul a1 a2) (Mul b1 b2) = f a1 b1 && f a2 b2
  liftEq f (Div a1 a2) (Div b1 b2) = f a1 b1 && f a2 b2
  liftEq _ _ _ = False

instance {-# OVERLAPPING #-} Show Expression where
  show = cata alg
    where
      alg (Application expr1 expr2) = "[" <> expr1 <> " " <> expr2 <> "]"
      alg (Lambda n expr) = "(λ" <> n <> "." <> expr <> ")"
      alg (Variable n) = n
      alg (Literal (LitInt v)) = show v
      alg (Literal (LitBool v)) = show v
      alg (Module n m) = "module" <> n <> "where\n" <> show (map snd m)
      alg (Definition n expr) = n <> " = " <> expr
      alg (Add expr1 expr2) = "(" <> expr1 <> "+" <> expr2 <> ")"
      alg (Sub expr1 expr2) = "(" <> expr1 <> "-" <> expr2 <> ")"
      alg (Mul expr1 expr2) = "(" <> expr1 <> "*" <> expr2 <> ")"
      alg (Div expr1 expr2) = "(" <> expr1 <> "/" <> expr2 <> ")"

litInt :: Integer -> Expression
litInt i = Fix . Literal $ LitInt i

litBool :: Bool -> Expression
litBool b = Fix . Literal $ LitBool b

mkApplication :: Expression -> Expression -> Expression
mkApplication f a = Fix $ Application f a

mkLambda :: Name -> Expression -> Expression
mkLambda n = Fix . Lambda n

mkVariable :: Name -> Expression
mkVariable = Fix . Variable

mkLiteral :: Lit -> Expression
mkLiteral = Fix . Literal

mkDefinition :: Name -> Expression -> Expression
mkDefinition n e = Fix $ Definition n e

mkModule :: Name -> [(Name, Expression)] -> Expression
mkModule n m = Fix $ Module n m

mkAdd :: Expression -> Expression -> Expression
mkAdd a b = Fix $ Add a b

mkSub :: Expression -> Expression -> Expression
mkSub a b = Fix $ Sub a b

mkMul :: Expression -> Expression -> Expression
mkMul a b = Fix $ Mul a b

mkDiv :: Expression -> Expression -> Expression
mkDiv a b = Fix $ Div a b

data Operator = AddOp | SubOp | MulOp | DivOp deriving (Show)
