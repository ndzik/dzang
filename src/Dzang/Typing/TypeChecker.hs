{-# LANGUAGE BangPatterns #-}

module Dzang.Typing.TypeChecker where

import Dzang.Language
import Dzang.Typing.Inferer
import Dzang.Typing.Solver
import Dzang.Typing.Types

runTypeChecker :: Expression -> PolyType
runTypeChecker expr = generalize $ apply s mt
  where
    (mt, cs) = evalInference expr
    !s = evalSolver cs

generalize :: MonoType -> PolyType
generalize = PType
