module Dzang.Test.SolverSpec where

import           Dzang.Typing.Solver
import           Dzang.Typing.Types
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                )
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = describe "Dzang ConstraintSolver" $ do
  it "unifies types"          testUnification
  it "solves constraints"     testSolver
  it "composes substitutions" testComposition

testUnification :: Expectation
testUnification = do
  unify (typevar "a1") (typevar "a2")
    `shouldBe` Right [(TypeVar "a1", typevar "a2")]
  unify (typevar "a1") (int :-> int)
    `shouldBe` Right [(TypeVar "a1", int :-> int)]
  unify (typevar "a1" :-> typevar "a2") (typevar "a3" :-> typevar "a4")
    `shouldBe` Right
                 [(TypeVar "a2", typevar "a4"), (TypeVar "a1", typevar "a3")]
  unify (typevar "a1" :-> typevar "a2") (typevar "a3" :-> int)
    `shouldBe` Right [(TypeVar "a2", int), (TypeVar "a1", typevar "a3")]

testSolver :: Expectation
testSolver = do
  evalSolver [] `shouldBe` Right []
  evalSolver
      [(typevar "a" :-> typevar "a", int :-> int), (typevar "a", typevar "b")]
    `shouldBe` Right [(TypeVar "b", int), (TypeVar "a", int)]
  evalSolver
      [ (typevar "a1", typevar "a2" :-> typevar "a4")
      , (typevar "a4", typevar "a3" :-> typevar "a5")
      ]
    `shouldBe` Right [ ( TypeVar "a1"
                 , typevar "a2" :-> (typevar "a3" :-> typevar "a5")
                 )
               , (TypeVar "a4", typevar "a3" :-> typevar "a5")
               ]

testComposition :: Expectation
testComposition = do
  [] `compose` [] `shouldBe` []
  [(TypeVar "a2", typevar "a4")]
    `compose`  [(TypeVar "a1", typevar "a3")]
    `shouldBe` [(TypeVar "a2", typevar "a4"), (TypeVar "a1", typevar "a3")]
  [(TypeVar "a", typevar "b")]
    `compose`  [(TypeVar "c", typevar "a" :-> int)]
    `shouldBe` [(TypeVar "a", typevar "b"), (TypeVar "c", typevar "b" :-> int)]
  [(TypeVar "a4", typevar "a3" :-> typevar "a5")]
    `compose`  [(TypeVar "a1", typevar "a2" :-> typevar "a4")]
    `shouldBe` [ (TypeVar "a4", typevar "a3" :-> typevar "a5")
               , ( TypeVar "a1"
                 , typevar "a2" :-> (typevar "a3" :-> typevar "a5")
                 )
               ]
  [(TypeVar "a1", typevar "a2" :-> typevar "a4")]
    `compose`  [(TypeVar "a4", typevar "a3" :-> typevar "a5")]
    `shouldBe` [ (TypeVar "a1", typevar "a2" :-> typevar "a4")
               , (TypeVar "a4", typevar "a3" :-> typevar "a5")
               ]
