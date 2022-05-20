module Dzang.Typing.TypeChecker where

import           Data.Bifunctor                 ( second )
import qualified Data.Set                      as S
import           Dzang.Language
import           Dzang.Typing.Error
import           Dzang.Typing.Inferer
import           Dzang.Typing.Solver
import           Dzang.Typing.Types

runTypeChecker :: TypingEnv -> Expression -> Either TypeError PolyType
runTypeChecker env expr = case evalInference env expr of
  Right (mt, cs) -> evalSolver cs >>= \s -> Right . normalize . generalize $ apply s mt
  Left  err      -> Left err

runTypeChecker'
  :: TypingEnv -> Expression -> Either TypeError (PolyType, TypingEnv)
runTypeChecker' env expr = case runInference' env expr of
  Right (mt, InfererState _ ds, cs) -> evalSolver cs >>= \s ->
    let env' = map (second $ generalize . apply s) ds ++ env
    in  Right (normalize . generalize $ apply s mt, env')
  Left err -> Left err

evalTypeChecker
  :: TypingEnv
  -> Expression
  -> Either TypeError (MonoType, PolyType, [Constraint], Substitution)
evalTypeChecker env expr = case evalInference env expr of
  Right (mt, cs) -> evalSolver cs
    >>= \s -> Right (mt, normalize . generalize $ apply s mt, cs, s)
  Left err -> Left err

generalize :: MonoType -> PolyType
generalize mt = case S.toList . freeTV $ mt of
  [] -> PType mt
  as -> ForAll as (PType mt)

normalize :: PolyType -> PolyType
normalize = go []
 where
  go :: Substitution -> PolyType -> PolyType
  go ss (PType mt    ) = PType $ apply ss mt
  go ss (ForAll as pt) = ForAll (map TypeVar ls) . go ss' $ pt
   where
    ls  = letters (length as)
    ss' = zip as (map typevar ls) `compose` ss

letters :: Int -> [String]
letters i = take i [ replicate n l | n <- [1 ..], l <- ['a' .. 'z'] ]
