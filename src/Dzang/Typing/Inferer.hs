{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Dzang.Typing.Inferer where

import Control.Monad.Except
import Control.Monad.RWS
import Data.Bifunctor (bimap)
import Data.Fix (Fix (..))
import Data.Functor ((<&>))
import Data.List (find)
import qualified Data.Set as S
import Dzang.AST
import Dzang.Typing.Error
import Dzang.Typing.Types

-- This module contains the `Inferer` which collects constraints and infers the
-- types of expressions written in `Dzang`.

-- `Inferer` runs over a `Dzang` expression and collects constraints about the
-- types used.
type Inferer a = RWST TypingEnv [Constraint] InfererState (Except TypeError) a

-- Constraint describes the constraint of a type by tying it together with a
-- respective type which allows to either generalize or instantiate the
-- possibly surrounding type scheme.
type Constraint = (MonoType, MonoType)

-- InfererState holds the index to `tvars` as a source of unique type names for
-- fresh type variables.
data InfererState = InfererState
  { freshTVarIndex :: Int,
    defs :: [(Name, MonoType)]
  }

runInference :: Expression -> Either TypeError MonoType
runInference expr =
  case runExcept $ runRWST (infer expr) [] (InfererState 0 []) of
    Right (r, _, _) -> Right r
    Left err -> Left err

runInference' ::
  TypingEnv -> Expression -> Either TypeError (MonoType, InfererState, [Constraint])
runInference' env expr = runExcept $ runRWST (infer expr) env (InfererState 0 [])

evalInference ::
  TypingEnv -> Expression -> Either TypeError (MonoType, [Constraint])
evalInference env expr =
  runExcept $ evalRWST (infer expr) env (InfererState 0 [])

freshTVar :: Inferer TypeVar
freshTVar = do
  i <- gets freshTVarIndex
  modify (\is -> is {freshTVarIndex = i + 1})
  return $ TypeVar (tvars !! i)

freshMTVar :: Inferer MonoType
freshMTVar = freshTVar <&> MType

freshPTVar :: Inferer PolyType
freshPTVar = freshMTVar <&> PType

tvars :: [String]
tvars = [b : show n | b <- ['a' .. 'z'], n <- [1 .. 10] :: [Integer]]

-- TypingEnv maps variables to their respective type scheme.
type TypingEnv = [(Name, PolyType)]

infer :: Expression -> Inferer MonoType
infer expr = case expr of
  Fix (Variable _ n) -> inferVar n
  Fix (Lambda _ n e) -> inferLambda n e
  Fix (Application _ e1 e2) -> inferApplication e1 e2
  Fix (Literal _ lit) -> inferLiteral lit
  Fix (Definition _ n e) -> inferDefinition n e
  Fix (Add _ lhs rhs) -> inferOperator AddOp lhs rhs
  Fix (Sub _ lhs rhs) -> inferOperator SubOp lhs rhs
  Fix (Mul _ lhs rhs) -> inferOperator MulOp lhs rhs
  Fix (Div _ lhs rhs) -> inferOperator DivOp lhs rhs

inferVar :: Name -> Inferer MonoType
inferVar n = ask >>= flip resolveType n

inferLambda :: Name -> Expression -> Inferer MonoType
inferLambda n e = do
  mt <- freshMTVar
  rt <- local (replaceTVar n $ ForAll [] (PType mt)) (infer e)
  return $ mt :-> rt

replaceTVar :: Name -> PolyType -> TypingEnv -> TypingEnv
replaceTVar n pt env = (n, pt) : filter (\(n', _) -> n' /= n) env

inferApplication :: Expression -> Expression -> Inferer MonoType
inferApplication e0 e1 = do
  mt0 <- infer e0
  mt1 <- infer e1
  mtv <- freshMTVar
  unify mt0 (mt1 :-> mtv)
  return mtv

inferLiteral :: Lit -> Inferer MonoType
inferLiteral (LitInt _) = return int
inferLiteral (LitBool _) = return bool

inferDefinition :: Name -> Expression -> Inferer MonoType
inferDefinition n expr = do
  mt <- freshMTVar
  rt <- local (replaceTVar n $ ForAll [] (PType mt)) (infer expr)
  unify mt rt
  trackDefinition n mt
  return mt

trackDefinition :: Name -> MonoType -> Inferer ()
trackDefinition n mt =
  modify (\is@(InfererState _ vs) -> is {defs = (n, mt) : vs})

inferOperator :: Operator -> Expression -> Expression -> Inferer MonoType
inferOperator _ lhs rhs = do
  tl <- infer lhs
  tr <- infer rhs
  unify (int :-> int) (tl :-> tr)
  return int

resolveType :: TypingEnv -> Name -> Inferer MonoType
resolveType env v = case find (\(n, _) -> n == v) env of
  Nothing -> throwError $ UnboundVariableError v
  -- Pulling the type of a variable from the environment requires instantiating
  -- the type scheme because the variable itself could either have a concrete
  -- type assigned to it already or be instantiated with a `TypeVariable`.
  Just (_, scheme) -> instantiate scheme

instantiate :: PolyType -> Inferer MonoType
instantiate pt = case pt of
  PType t -> return t
  -- Assign a new `MonoType` type variable for each bound type variable. To
  -- track this assignment we are required to substitue each occurrence of
  -- NON-FREE typevariables within `pt` and possibly rename if conflicts
  -- happen.
  fapt -> instantiate' [] fapt
    where
      instantiate' s (PType t) = return $ apply s t
      instantiate' s (ForAll vs t) = do
        vs' <- mapM (const freshMTVar) vs
        let substitution = zip vs vs' in instantiate' (s ++ substitution) t

type Substitution = [(TypeVar, MonoType)]

class Substitutable a where
  apply :: Substitution -> a -> a
  freeTV :: a -> S.Set TypeVar

instance Substitutable MonoType where
  apply s mt = case mt of
    -- Typevariables are either substituted or stay the same if not found in
    -- the substitution list.
    MType tv -> case find (\(tv', _) -> tv == tv') s of
      Nothing -> MType tv
      Just (_, stv) -> stv
    -- Concrete types cannot be substituted so they will always stay the same.
    MConcreteType ct -> MConcreteType ct
    -- Arrow types (functions) stay functions and the substitution is
    -- propagated to their argument(-types).
    lhs :-> rhs -> apply s lhs :-> apply s rhs
    -- Arrow types are just a special case for type constructors but they are
    -- REQUIRED in HM, which is why they have an extra representation. MTypeCon
    -- are handled just like arrow types.
    MTypeCon mtc -> MTypeCon (apply s mtc)
  freeTV mt = case mt of
    MType tv -> S.singleton tv
    MConcreteType _ -> S.empty
    lhs :-> rhs -> freeTV lhs `S.union` freeTV rhs
    MTypeCon mtc -> freeTV mtc

instance Substitutable PolyType where
  apply s pt = case pt of
    PType mt -> PType $ apply s mt
    -- Typeschemes remove their own typevariables over which they quantify from
    -- the substitution list because of namecapturing. The substition is applied
    -- afterwards
    ForAll as pt' -> ForAll as (apply [] pt')
  freeTV pt = case pt of
    PType mt -> freeTV mt
    ForAll as pt' ->
      S.fromList [tv | a <- as, tv <- S.toList $ freeTV pt', a /= tv]

instance Substitutable Constraint where
  apply s c = bimap (apply s) (apply s) c
  freeTV (t1, t2) = freeTV t1 `S.union` freeTV t2

instance Substitutable [Constraint] where
  apply s cs = map (apply s) cs
  freeTV cs = foldl S.union S.empty $ map freeTV cs

-- unify tracks relationship (constraint) between the given monotypes.
unify :: MonoType -> MonoType -> Inferer ()
unify a b = tell [(a, b)]
