module Dzang.Test.TypeCheckerSpec where

import Control.Exception
import Dzang.Language
import Dzang.Typing.TypeChecker
import Dzang.Typing.Types
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = describe "Dzang TypeChecker" $ do
  it "resolves correct types" $ do
    runTypeChecker (litInt 10) `shouldBe` PType int
    runTypeChecker (Lambda "x" (Add (Variable "x") (litInt 1))) `shouldBe` PType (int :-> int)
    runTypeChecker (Application (Lambda "x" (Add (Variable "x") (litInt 1))) (litInt 2)) `shouldBe` PType int
    runTypeChecker (Lambda "x" (Lambda "y" (Variable "x"))) `shouldBe` PType (typevar "a1" :-> (typevar "a2" :-> typevar "a1"))
    runTypeChecker (Lambda "x" (Lambda "y" (Add (Variable "x") (Variable "y")))) `shouldBe` PType (int :-> (int :-> int))
  it "rejects ill-typed expressions" testErrors

testErrors :: Expectation
testErrors = do
  evaluate (runTypeChecker (Lambda "x" (Application (Variable "x") (Variable "x")))) `shouldThrow` errorCall "occurs check: cannot create infinite type"
  evaluate (runTypeChecker (Application (Lambda "x" (Add (Variable "x") (litBool True))) (litInt 2))) `shouldThrow` errorCall "failed unification for int ~ bool"