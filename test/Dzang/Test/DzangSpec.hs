module Dzang.Test.DzangSpec where

import Dzang.Interpreter.Interpreter
import Dzang.Test.Interpreter
import Dzang.Typing.Types
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = describe "Dzang Language" $ do
  it "parses & typechecks valid expressions" testValidExprPT
  it "parses & typechecks & evalutes valid expressions" testValidExprPTE

testValidExprPT :: Expectation
testValidExprPT = do
  parseType "λx.x"
    `shouldBe` ForAll [TypeVar "a"] (PType $ typevar "a" :-> typevar "a")
  parseType "λx.λy.x"
    `shouldBe` ForAll
      [TypeVar "a", TypeVar "b"]
      (PType $ typevar "a" :-> (typevar "b" :-> typevar "a"))
  parseType "λx.λy.x true"
    `shouldBe` ForAll [TypeVar "a"] (PType $ typevar "a" :-> bool)
  parseType "λx.λy.x true 1" `shouldBe` PType bool
  parseType "λx.λy.1"
    `shouldBe` ForAll
      [TypeVar "a", TypeVar "b"]
      (PType $ typevar "a" :-> (typevar "b" :-> int))
  parseType "λx.λy.1 false"
    `shouldBe` ForAll [TypeVar "a"] (PType $ typevar "a" :-> int)
  parseType "λx.λy.1 false true" `shouldBe` PType int
  parseType "λf.λx.λy.(f x y)"
    `shouldBe` ForAll
      [TypeVar "a", TypeVar "b", TypeVar "c"]
      ( PType $
          (typevar "a" :-> (typevar "b" :-> typevar "c"))
            :-> (typevar "a" :-> (typevar "b" :-> typevar "c"))
      )
  parseType "λf.λx.λy.(f x y) (λa.λb.(a+b))"
    `shouldBe` PType (int :-> (int :-> int))

testValidExprPTE :: Expectation
testValidExprPTE = do
  parseTypeEval "λx.x+1 1" `shouldBe` (VInt 2, PType int)
  parseTypeEval "λx.λy.x true 1" `shouldBe` (VBool True, PType bool)
