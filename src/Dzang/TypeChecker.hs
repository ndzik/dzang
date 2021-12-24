{-# LANGUAGE LambdaCase #-}
module Dzang.TypeChecker where

import           Control.Monad.State
import           Dzang.Language

data Type = TBool
          | TInt
          | TArr Name Type Type
          | TPoly Name
          deriving (Eq, Show)

type Environment = [(Name, Type)]
type Checker a = State Environment a

runChecker :: String -> Type
runChecker input = evalState (check $ parseDzang input) []

check :: Expression -> Checker Type
check (Literal (LitInt _)) = return TInt
check (Literal (LitBool _)) = return TBool
check (Variable name) = checkVariable name
check (Add lhs rhs) = checkPrimitive lhs rhs "mismatched types on addition"
check (Sub lhs rhs) = checkPrimitive lhs rhs "mismatched types on subtraction"
check (Mul lhs rhs) = checkPrimitive lhs rhs "mismatched types on multipli."
check (Div lhs rhs) = checkPrimitive lhs rhs "mismatched types on division"
check (Lambda name expr) = checkLambda name expr
check (Application fun val) = checkApplication fun val
check (Module _ _) = error "modules have no type"
check (Definition _ expr) = check expr

checkPrimitive :: Expression -> Expression -> String -> Checker Type
checkPrimitive lhs rhs msg = do
  lht <- check lhs
  rht <- check rhs
  if lht `matches` rht then specify lht rht else error msg
 where
  specify :: Type -> Type -> Checker Type
  specify (TPoly name) r            = modify (\s -> (name, r) : s) >> return r
  specify l            (TPoly name) = modify (\s -> (name, l) : s) >> return l
  specify l r | l == r    = return l
              | otherwise = error "incompatible types"

checkVariable :: Name -> Checker Type
checkVariable name = gets (lookup name) >>= \case
  Nothing -> error "unsaturated variable found"
  Just t  -> return t

checkLambda :: Name -> Expression -> Checker Type
checkLambda name expr = modify (\s -> (name, TPoly name) : s) >> do
  returnType <- check expr
  inputType  <- gets (lookup name) >>= \case
    Nothing -> error "variable not in scope"
    Just v  -> return v
  return $ TArr name inputType returnType

checkApplication :: Expression -> Expression -> Checker Type
checkApplication fun input = do
  funType   <- check fun
  inputType <- check input
  case funType of
    TArr name expInputType returnType ->
      if not (inputType `matches` expInputType)
        then error "type mismatch on function application"
        else modify (\s -> (name, expInputType) : s) >> return returnType
    _ -> error "type error applying argument to non function"

matches :: Type -> Type -> Bool
matches TBool        TBool          = True
matches TInt         TInt           = True
matches (TArr _ a b) (TArr _ a' b') = a `matches` a' && b `matches` b'
matches (TPoly _   ) _              = True
matches _            (TPoly _)      = True
matches _            _              = False
