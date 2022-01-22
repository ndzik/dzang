module Dzang.Test.SolverSpec where

import Dzang.Typing.Solver
import Dzang.Typing.Types
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = describe "Dzang ConstraintSolver" $ do
  it "unifies types" testUnification
  it "solves constraints" testSolver

testUnification :: Expectation
testUnification = do
  unify (typevar "a1") (typevar "a2") `shouldBe` [(TypeVar "a1", typevar "a2")]
  unify (typevar "a1") (int :-> int)
    `shouldBe` [(TypeVar "a1", int :-> int)]
  unify (typevar "a1" :-> typevar "a2") (typevar "a3" :-> typevar "a4")
    `shouldBe` [(TypeVar "a2", typevar "a4"), (TypeVar "a1", typevar "a3")]
  unify (typevar "a1" :-> typevar "a2") (typevar "a3" :-> int)
    `shouldBe` [(TypeVar "a2", int), (TypeVar "a1", typevar "a3")]

testSolver :: Expectation
testSolver = do
  evalSolver [] `shouldBe` []
  evalSolver [(typevar "a", typevar "b"), (typevar "a" :-> typevar "a", int :-> int)] `shouldBe` [(TypeVar "a", typevar "b"), (TypeVar "b", int)]
  evalSolver [(typevar "a" :-> typevar "a", int :-> int), (typevar "a", typevar "b")] `shouldBe` [(TypeVar "a", int), (TypeVar "b", int)]
