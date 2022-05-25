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

defPos :: (Int, Int)
defPos = (0,0)

testPrimitives :: Expectation
testPrimitives = do
  evalInference [] (mkLiteral defPos (LitInt 1)) `shouldBe` Right (int, [])
  evalInference [] (mkLiteral defPos (LitBool False)) `shouldBe` Right (bool, [])
  evalInference [] (mkLambda defPos "x" (mkVariable defPos "x"))
    `shouldBe` Right (typevar "a1" :-> typevar "a1", [])
  evalInference [] (mkLambda defPos "x" (mkLiteral defPos (LitInt 1)))
    `shouldBe` Right (typevar "a1" :-> int, [])
  evalInference [] (mkLambda defPos "x" (mkLambda defPos "y" (mkLiteral defPos (LitInt 1))))
    `shouldBe` Right (typevar "a1" :-> (typevar "a2" :-> int), [])
  evalInference [] (mkLambda defPos "x" (mkAdd defPos (litInt defPos 1) (mkVariable defPos "x")))
    `shouldBe` Right (typevar "a1" :-> int, [(int :-> int, int :-> typevar "a1")])
  -- λx.1+x 1 = 2
  -- a1 -> int
  -- (int -> int) ~ (int -> a1) <- inferOperator
  -- (a1 -> int) ~ (int -> a2) <- inferApplication
  evalInference
    []
    (mkApplication defPos (mkLambda defPos "x" (mkAdd defPos (litInt defPos 1) (mkVariable defPos "x"))) (litInt defPos 1))
    `shouldBe` Right
      ( typevar "a2",
        [ (int :-> int, int :-> typevar "a1"),
          (typevar "a1" :-> int, int :-> typevar "a2")
        ]
      )

testIntOperations :: Expectation
testIntOperations = do
  runInference (mkAdd defPos (mkLiteral defPos (LitInt 1)) (mkLiteral defPos (LitInt 1))) `shouldBe` Right int
  evalInference [] (mkDiv defPos (mkLiteral defPos (LitInt 1)) (mkLiteral defPos (LitBool False)))
    `shouldBe` Right (int, [(int :-> int, int :-> bool)])

testFunctionUnification :: Expectation
testFunctionUnification = do
  runExcept (evalRWST (unify (int :-> int) (int :-> bool)) [] (InfererState 0 []))
    `shouldBe` Right ((), [(int :-> int, int :-> bool)])
