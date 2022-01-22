module Dzang.Test.InfererSpec where

import           Control.Monad.RWS
import           Dzang.Language
import           Dzang.Typing.Inferer
import           Dzang.Typing.Types
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                )
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "Dzang Typeinference" $ do
    it "infers primitives"            testPrimitives
    it "infers built-in int-operator" testIntOperations
  describe "Unification" $ do
    it "unifies functions"      testFunctionUnification
    it "composes substitutions" testComposition

testPrimitives :: Expectation
testPrimitives = do
  evalInference (Literal (LitInt 1)) `shouldBe` (int, [])
  evalInference (Literal (LitBool False)) `shouldBe` (bool, [])
  evalInference (Lambda "x" (Variable "x"))
    `shouldBe` (typevar "a1" :-> typevar "a1", [])
  evalInference (Lambda "x" (Literal (LitInt 1)))
    `shouldBe` (typevar "a1" :-> int, [])
  evalInference (Lambda "x" (Lambda "y" (Literal (LitInt 1))))
    `shouldBe` (typevar "a1" :-> (typevar "a2" :-> int), [])
  evalInference (Lambda "x" (Add (litInt 1) (Variable "x")))
    `shouldBe` (typevar "a1" :-> int, [(int :-> int, int :-> typevar "a1")])
  -- λx.1+x 1 = 2
  -- a1 -> int
  -- (int -> int) ~ (int -> a1) <- inferOperator
  -- (a1 -> int) ~ (int -> a2) <- inferApplication
  evalInference
      (Application (Lambda "x" (Add (litInt 1) (Variable "x"))) (litInt 1))
    `shouldBe` ( typevar "a2"
               , [ (int :-> int         , int :-> typevar "a1")
                 , (typevar "a1" :-> int, int :-> typevar "a2")
                 ]
               )

testIntOperations :: Expectation
testIntOperations = do
  runInference (Add (Literal (LitInt 1)) (Literal (LitInt 1))) `shouldBe` int
  evalInference (Div (Literal (LitInt 1)) (Literal (LitBool False)))
    `shouldBe` (int, [(int :-> int, int :-> bool)])

testFunctionUnification :: Expectation
testFunctionUnification = do
  snd (evalRWS (unify (int :-> int) (int :-> bool)) [] (InfererState 0))
    `shouldBe` [(int :-> int, int :-> bool)]

testComposition :: Expectation
testComposition = do
  [] `compose` [] `shouldBe` []
  [(TypeVar "a2", typevar "a4")]
    `compose`  [(TypeVar "a1", typevar "a3")]
    `shouldBe` [(TypeVar "a2", typevar "a4"), (TypeVar "a1", typevar "a3")]
  [(TypeVar "a", typevar "b")]
    `compose`  [(TypeVar "c", typevar "a" :->int)]
    `shouldBe` [ (TypeVar "a", typevar "b")
               , (TypeVar "c", typevar "b" :-> int)
               ]
