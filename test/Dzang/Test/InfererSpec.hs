module Dzang.Test.InfererSpec where

import Control.Monad.Except
import Control.Monad.RWS
import Dzang.AST
import Dzang.Typing.Inferer
import Dzang.Typing.Types
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "Dzang Typeinference" $ do
    it "infers primitives" testPrimitives
    it "infers built-in int-operator" testIntOperations
  describe "Unification" $ do
    it "unifies functions" testFunctionUnification

testPrimitives :: Expectation
testPrimitives = do
  evalInference [] (mkLiteral (LitInt 1)) `shouldBe` Right (int, [])
  evalInference [] (mkLiteral (LitBool False)) `shouldBe` Right (bool, [])
  evalInference [] (mkLambda "x" (mkVariable "x"))
    `shouldBe` Right (typevar "a1" :-> typevar "a1", [])
  evalInference [] (mkLambda "x" (mkLiteral (LitInt 1)))
    `shouldBe` Right (typevar "a1" :-> int, [])
  evalInference [] (mkLambda "x" (mkLambda "y" (mkLiteral (LitInt 1))))
    `shouldBe` Right (typevar "a1" :-> (typevar "a2" :-> int), [])
  evalInference [] (mkLambda "x" (mkAdd (litInt 1) (mkVariable "x")))
    `shouldBe` Right (typevar "a1" :-> int, [(int :-> int, int :-> typevar "a1")])
  -- λx.1+x 1 = 2
  -- a1 -> int
  -- (int -> int) ~ (int -> a1) <- inferOperator
  -- (a1 -> int) ~ (int -> a2) <- inferApplication
  evalInference
    []
    (mkApplication (mkLambda "x" (mkAdd (litInt 1) (mkVariable "x"))) (litInt 1))
    `shouldBe` Right
      ( typevar "a2",
        [ (int :-> int, int :-> typevar "a1"),
          (typevar "a1" :-> int, int :-> typevar "a2")
        ]
      )

testIntOperations :: Expectation
testIntOperations = do
  runInference (mkAdd (mkLiteral (LitInt 1)) (mkLiteral (LitInt 1))) `shouldBe` Right int
  evalInference [] (mkDiv (mkLiteral (LitInt 1)) (mkLiteral (LitBool False)))
    `shouldBe` Right (int, [(int :-> int, int :-> bool)])

testFunctionUnification :: Expectation
testFunctionUnification = do
  runExcept (evalRWST (unify (int :-> int) (int :-> bool)) [] (InfererState 0 []))
    `shouldBe` Right ((), [(int :-> int, int :-> bool)])
