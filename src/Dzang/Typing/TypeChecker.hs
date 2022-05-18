{-# LANGUAGE BangPatterns #-}

module Dzang.Typing.TypeChecker where

import qualified Data.Set as S
import Dzang.Language
import Dzang.Typing.Inferer
import Dzang.Typing.Solver
import Dzang.Typing.Types
import Data.Bifunctor (second)

runTypeChecker :: TypingEnv -> Expression -> PolyType
runTypeChecker env expr = normalize . generalize $ apply s mt
  where
    (mt, cs) = evalInference env expr
    !s = evalSolver cs

runTypeChecker' :: TypingEnv -> Expression -> (PolyType, TypingEnv)
runTypeChecker' env expr = (normalize . generalize $ apply s mt, env')
  where
    (mt, InfererState _ ds, cs) = runInference' expr
    !s = evalSolver cs
    env' = map (second $ generalize . apply s) ds ++ env

evalTypeChecker :: TypingEnv -> Expression -> (MonoType, PolyType, [Constraint], Substitution)
evalTypeChecker env expr = (mt, normalize . generalize $ apply s mt, cs, s)
  where
    (mt, cs) = evalInference env expr
    !s = evalSolver cs

generalize :: MonoType -> PolyType
generalize mt = case S.toList . freeTV $ mt of
  [] -> PType mt
  as -> ForAll as (PType mt)

normalize :: PolyType -> PolyType
normalize = go []
  where
    go :: Substitution -> PolyType -> PolyType
    go ss (PType mt) = PType $ apply ss mt
    go ss (ForAll as pt) = ForAll (map TypeVar ls) . go ss' $ pt
      where
        ls = letters (length as)
        ss' = zip as (map typevar ls) `compose` ss

letters :: Int -> [String]
letters i = take i [replicate n l | n <- [1 ..], l <- ['a' .. 'z']]
