{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Dzang.Typing.Inferer where

import Control.Monad.RWS
import Data.Bifunctor (bimap, second)
import Data.Functor ((<&>))
import Data.List (find)
import Dzang.Language
import Dzang.Typing.Types
import Text.Printf

-- This module contains the `Inferer` which collects constraints and infers the
-- types of expressions written in `Dzang`.

-- `Inferer` runs over a `Dzang` expression and collects constraints about the
-- types used.
type Inferer a = RWS TypingEnv [Constraint] InfererState a

-- Constraint describes the constraint of a type by tying it together with a
-- respective type which allows to either generalize or instantiate the
-- possibly surrounding type scheme.
type Constraint = (MonoType, MonoType)

-- InfererState holds the index to `tvars` as a source of unique type names for
-- fresh type variables.
newtype InfererState = InfererState
  { freshTVarIndex :: Int
  }

runInference :: Expression -> MonoType
runInference expr = t
  where
    (t, _, _) = runRWS (infer expr) [] (InfererState 0)

evalInference :: Expression -> (MonoType, [Constraint])
evalInference expr = evalRWS (infer expr) [] (InfererState 0)

freshTVar :: Inferer TypeVar
freshTVar = do
  i <- gets freshTVarIndex
  put (InfererState {freshTVarIndex = i + 1})
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
  Variable n -> inferVar n
  Lambda n e -> inferLambda n e
  Application e1 e2 -> inferApplication e1 e2
  Literal lit -> inferLiteral lit
  Module _ _ -> error "modules do not have a type"
  Definition _ _ -> error "definition not supported (yet)"
  Add lhs rhs -> inferOperator AddOp lhs rhs
  Sub lhs rhs -> inferOperator SubOp lhs rhs
  Mul lhs rhs -> inferOperator MulOp lhs rhs
  Div lhs rhs -> inferOperator DivOp lhs rhs

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

inferOperator :: Operator -> Expression -> Expression -> Inferer MonoType
inferOperator _ lhs rhs = do
  tl <- infer lhs
  tr <- infer rhs
  -- Trying to unify `int -> int` with `lhs + rhs` should result in `int`. Due
  -- to laziness we have to force evaluation to WHNF with `!`.
  unify (int :-> int) (tl :-> tr)
  return int

resolveType :: TypingEnv -> Name -> Inferer MonoType
resolveType env v = case find (\(n, _) -> n == v) env of
  Nothing -> error $ printf "unbound variable: %s" v
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
  freeTV :: a -> [TypeVar]

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
    MType tv -> [tv]
    MConcreteType _ -> []
    lhs :-> rhs -> freeTV lhs ++ freeTV rhs
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
    ForAll as pt' -> [tv | a <- as, tv <- freeTV pt', a /= tv]

instance Substitutable Constraint where
  apply s c = bimap (apply s) (apply s) c
  freeTV (t1, t2) = freeTV t1 ++ freeTV t2

instance Substitutable [Constraint] where
  apply s cs = map (apply s) cs
  freeTV cs = concatMap freeTV cs

-- unify tracks relationship (constraint) between the given monotypes.
unify :: MonoType -> MonoType -> Inferer ()
unify a b = tell [(a, b)]

compose :: Substitution -> Substitution -> Substitution
compose s2 s1 = s2 ++ map (second (apply s2)) s1
