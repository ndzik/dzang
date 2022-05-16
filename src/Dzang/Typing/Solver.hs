{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Dzang.Typing.Solver where

import Control.Monad.State
import Data.Bifunctor (second)
import Dzang.Typing.Inferer
  ( Constraint,
    Substitutable (..),
    Substitution,
    apply,
  )
import Dzang.Typing.Types
import Text.Printf

-- Solver monad responsible for resolving a list of constraints.
type Solver a = State [Constraint] a

evalSolver :: [Constraint] -> Substitution
evalSolver = evalState (solve []) . reverse

-- solve checks the list of constraints for consistency resulting in a
-- substitution which can be applied to a PolyType.
solve :: Substitution -> Solver Substitution
solve sub =
  get >>= \case
    [] -> return sub
    ((t1, t2) : cs) -> let s = unify t1 t2 in put (apply s cs) >> solve (s `compose` sub)

-- unify tries to find a `Substitution` to unify MonoType `a` with MonoType
-- `b`.
unify :: MonoType -> MonoType -> Substitution
unify t1 t2 | t1 == t2 = []
unify (MType tv) t2 = bind tv t2
unify t1 (MType tv) = bind tv t1
unify (mt1 :-> mt2) (mt1' :-> mt2') =
  -- Use bang-patterns here because laziness leads to the potential issue where
  -- `s1 == []` and `compose`ing this with `s2` will lead to `s2` never being
  -- evaluated.
  let !s1 = unify mt1 mt1'
      !s2 = unify (apply s1 mt2) (apply s1 mt2')
   in s2 `compose` s1
unify (MTypeCon mt) (MTypeCon mt') = unify mt mt'
unify t1 t2 =
  error $ printf "failed unification for %s ~ %s" (show t1) (show t2)

compose :: Substitution -> Substitution -> Substitution
compose s2 s1 = s2 ++ map (second (apply s2)) s1

-- bind returns a Substitution for the given `TypeVar` with the `MonoType` iff
-- the given typevariable is not free.
bind :: TypeVar -> MonoType -> Substitution
bind tv mt =
  let ftv = freeTV mt
   in if tv `elem` ftv
        then error "occurs check: cannot create infinite type"
        else [(tv, mt)]
