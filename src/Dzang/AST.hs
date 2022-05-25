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
  = Application (Int, Int) r r
  | -- (λa.12+r)
    Lambda (Int, Int) Name r
  | Variable (Int, Int) Name
  | Literal (Int, Int) Lit
  | Definition (Int, Int) Name r
  | -- Do I want it to be possible to bind `Module`s to variables?
    Module Name [(Name, r)]
  | -- Primitives
    Add (Int, Int) r r
  | Sub (Int, Int) r r
  | Mul (Int, Int) r r
  | Div (Int, Int) r r
  deriving (Show, Eq, Functor)

-- Alternatively with {-#LANGUAGE TemplateHaskell#-}:

-- $(deriveEq1 ''ExpressionF)

instance Eq1 ExpressionF where
  liftEq f (Application pos1 a1 a2) (Application pos2 b1 b2) = pos1 == pos2 && f a1 b1 && f a2 b2
  liftEq f (Lambda pos1 na a) (Lambda pos2 nb b) = pos1 == pos2 && na == nb && f a b
  liftEq _ (Variable pos1 a) (Variable pos2 b) = pos1 == pos2 && a == b
  liftEq _ (Literal pos1 a) (Literal pos2 b) = pos1 == pos2 && a == b
  liftEq f (Definition pos1 na a) (Definition pos2 nb b) = pos1 == pos2 && na == nb && f a b
  liftEq f (Module na a) (Module nb b) = na == nb && length a == length b && and (zipWith (curry (\((n, c), (n', c')) -> n == n' && f c c')) a b)
  liftEq f (Add pos1 a1 a2) (Add pos2 b1 b2) = pos1 == pos2 && f a1 b1 && f a2 b2
  liftEq f (Sub pos1 a1 a2) (Sub pos2 b1 b2) = pos1 == pos2 && f a1 b1 && f a2 b2
  liftEq f (Mul pos1 a1 a2) (Mul pos2 b1 b2) = pos1 == pos2 && f a1 b1 && f a2 b2
  liftEq f (Div pos1 a1 a2) (Div pos2 b1 b2) = pos1 == pos2 && f a1 b1 && f a2 b2
  liftEq _ _ _ = False

instance {-# OVERLAPPING #-} Show Expression where
  show = cata alg
    where
      alg (Application (col, line) expr1 expr2) = "{" <> show col <> "," <> show line <> "}" <> "[" <> expr1 <> " " <> expr2 <> "]"
      alg (Lambda (col, line) n expr) = "{" <> show col <> "," <> show line <> "}" <> "(λ" <> n <> "." <> expr <> ")"
      alg (Variable (col, line) n) = "{" <> show col <> "," <> show line <> "}" <> n
      alg (Literal (col, line) (LitInt v)) = "{" <> show col <> "," <> show line <> "}" <> show v
      alg (Literal (col, line) (LitBool v)) = "{" <> show col <> "," <> show line <> "}" <> show v
      alg (Module n m) = "module" <> n <> "where\n" <> show (map snd m)
      alg (Definition (col, line) n expr) = "{" <> show col <> "," <> show line <> "}" <> n <> " = " <> expr
      alg (Add (col, line) expr1 expr2) = "{" <> show col <> "," <> show line <> "}" <> "(" <> expr1 <> "+" <> expr2 <> ")"
      alg (Sub (col, line) expr1 expr2) = "{" <> show col <> "," <> show line <> "}" <> "(" <> expr1 <> "-" <> expr2 <> ")"
      alg (Mul (col, line) expr1 expr2) = "{" <> show col <> "," <> show line <> "}" <> "(" <> expr1 <> "*" <> expr2 <> ")"
      alg (Div (col, line) expr1 expr2) = "{" <> show col <> "," <> show line <> "}" <> "(" <> expr1 <> "/" <> expr2 <> ")"

litInt :: (Int, Int) -> Integer -> Expression
litInt p i = Fix . Literal p $ LitInt i

litBool :: (Int, Int) -> Bool -> Expression
litBool p b = Fix . Literal p $ LitBool b

mkApplication :: (Int, Int) -> Expression -> Expression -> Expression
mkApplication p f a = Fix $ Application p f a

mkLambda :: (Int, Int) -> Name -> Expression -> Expression
mkLambda p n = Fix . Lambda p n

mkVariable :: (Int, Int) -> Name -> Expression
mkVariable p = Fix . Variable p

mkLiteral :: (Int, Int) -> Lit -> Expression
mkLiteral p = Fix . Literal p

mkDefinition :: (Int, Int) -> Name -> Expression -> Expression
mkDefinition p n e = Fix $ Definition p n e

mkModule :: Name -> [(Name, Expression)] -> Expression
mkModule n m = Fix $ Module n m

mkAdd :: (Int, Int) -> Expression -> Expression -> Expression
mkAdd p a b = Fix $ Add p a b

mkSub :: (Int, Int) -> Expression -> Expression -> Expression
mkSub p a b = Fix $ Sub p a b

mkMul :: (Int, Int) -> Expression -> Expression -> Expression
mkMul p a b = Fix $ Mul p a b

mkDiv :: (Int, Int) -> Expression -> Expression -> Expression
mkDiv p a b = Fix $ Div p a b

data Operator = AddOp | SubOp | MulOp | DivOp deriving (Show)
