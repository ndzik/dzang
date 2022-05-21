module Dzang.Test.TypeCheckerSpec where

import Dzang.AST
import Dzang.Typing.Error
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
    runTypeChecker [] (litInt 10) `shouldBe` Right (PType int)
    runTypeChecker [] (mkLambda "x" (mkAdd (mkVariable "x") (litInt 1)))
      `shouldBe` Right (PType (int :-> int))
    runTypeChecker
      []
      (mkApplication (mkLambda "x" (mkAdd (mkVariable "x") (litInt 1))) (litInt 2))
      `shouldBe` Right (PType int)
    runTypeChecker [] (mkLambda "x" (mkLambda "y" (mkVariable "x")))
      `shouldBe` Right
        ( ForAll
            [TypeVar "a", TypeVar "b"]
            (PType $ typevar "a" :-> (typevar "b" :-> typevar "a"))
        )
    runTypeChecker
      []
      (mkLambda "x" (mkLambda "y" (mkAdd (mkVariable "x") (mkVariable "y"))))
      `shouldBe` Right (PType (int :-> (int :-> int)))
  it "rejects ill-typed expressions" testErrors
  it "updates TypingEnv" $ do
    runTypeChecker' [] (mkDefinition "a" (litInt 1))
      `shouldBe` Right (PType int, [("a", PType int)])

testErrors :: Expectation
testErrors = do
  runTypeChecker [] (mkLambda "x" (mkApplication (mkVariable "x") (mkVariable "x")))
    `shouldBe` Left
      ( InfiniteTypeError
          (TypeVar "a1")
          (typevar "a1" :-> typevar "a2")
      )
  runTypeChecker
    []
    (mkApplication (mkLambda "x" (mkAdd (mkVariable "x") (litBool True))) (litInt 2))
    `shouldBe` Left (MismatchedTypesError int bool)
