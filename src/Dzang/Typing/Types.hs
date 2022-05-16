{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Dzang.Typing.Types where

newtype TypeVar = TypeVar String deriving (Eq, Ord)

data MonoType
  = MType TypeVar -- a. Typevariables which are NOT monomorphic.
  | MConcreteType String -- Monomorphic types. E.g. String, Integer
  | MTypeCon MonoType -- C a b. E.g. Map a b
  | MonoType :-> MonoType -- a -> a
  deriving (Eq)

data PolyType
  = PType MonoType
  | ForAll [TypeVar] PolyType
  deriving (Eq)

instance Show MonoType where
  show (MType (TypeVar n)) = n
  show (MConcreteType n) = n
  show (MTypeCon t) = "C " <> show t
  show (a :-> b) = "( " <> show a <> " -> " <> show b <> " )"

instance Show PolyType where
  show (PType mt) = "{ " <> show mt <> " }"
  show (ForAll [] pt) = show pt
  show (ForAll as pt) = "∀" <> unwords (map show as) <> "." <> show pt

instance Show TypeVar where
  show (TypeVar n) = n

int :: MonoType
int = MConcreteType "int"

bool :: MonoType
bool = MConcreteType "bool"

typevar :: String -> MonoType
typevar n = MType (TypeVar n)
