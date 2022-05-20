module Dzang.Test.TypeCheckerSpec where

import           Dzang.Language
import           Dzang.Typing.Error
import           Dzang.Typing.TypeChecker
import           Dzang.Typing.Types
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                )
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = describe "Dzang TypeChecker" $ do
  it "resolves correct types" $ do
    runTypeChecker [] (litInt 10) `shouldBe` Right (PType int)
    runTypeChecker [] (Lambda "x" (Add (Variable "x") (litInt 1)))
      `shouldBe` Right (PType (int :-> int))
    runTypeChecker
        []
        (Application (Lambda "x" (Add (Variable "x") (litInt 1))) (litInt 2))
      `shouldBe` Right (PType int)
    runTypeChecker [] (Lambda "x" (Lambda "y" (Variable "x"))) `shouldBe` Right
      (ForAll [TypeVar "a", TypeVar "b"]
              (PType $ typevar "a" :-> (typevar "b" :-> typevar "a"))
      )
    runTypeChecker
        []
        (Lambda "x" (Lambda "y" (Add (Variable "x") (Variable "y"))))
      `shouldBe` Right (PType (int :-> (int :-> int)))
  it "rejects ill-typed expressions" testErrors

testErrors :: Expectation
testErrors = do
  runTypeChecker [] (Lambda "x" (Application (Variable "x") (Variable "x")))
    `shouldBe` Left
                 (InfiniteTypeError (TypeVar "a1")
                                    (typevar "a1" :-> typevar "a2")
                 )
  runTypeChecker
      []
      (Application (Lambda "x" (Add (Variable "x") (litBool True))) (litInt 2))
    `shouldBe` Left (MismatchedTypesError int bool)
