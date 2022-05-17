module Dzang.Typing.Error where

import Dzang.Language
import Dzang.Typing.Types
import Text.Printf

data TypeError
  = UnsupportedExprError Expression
  | UnboundVariableError Name
  | MismatchedTypesError MonoType MonoType
  | InfiniteTypeError TypeVar MonoType
  deriving (Eq)

instance Show TypeError where
  show (UnsupportedExprError (Module _ _)) = "modules do not have a type"
  show (UnsupportedExprError (Definition _ _)) = "definitions do not have a type"
  show (UnsupportedExprError expr) = printf "expression does not have a type: %s" (show expr)
  show (UnboundVariableError n) = "unbound variable encountered: " <> n
  show (MismatchedTypesError mt1 mt2) = printf "unification error: mismatched types %s ~ %s" (show mt1) (show mt2)
  show (InfiniteTypeError tv mt) = printf "occurs check: cannot create infinite type: %s => %s" (show tv) (show mt)
