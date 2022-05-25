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

defPos :: (Int, Int)
defPos = (0,0)

spec :: Spec
spec = describe "Dzang TypeChecker" $ do
  it "resolves correct types" $ do
    runTypeChecker [] (litInt defPos 10) `shouldBe` Right (PType int)
    runTypeChecker [] (mkLambda defPos "x" (mkAdd defPos (mkVariable defPos "x") (litInt defPos 1)))
      `shouldBe` Right (PType (int :-> int))
    runTypeChecker
      []
      (mkApplication defPos (mkLambda defPos "x" (mkAdd defPos (mkVariable defPos "x") (litInt defPos 1))) (litInt defPos 2))
      `shouldBe` Right (PType int)
    runTypeChecker [] (mkLambda defPos "x" (mkLambda defPos "y" (mkVariable defPos "x")))
      `shouldBe` Right
        ( ForAll
            [TypeVar "a", TypeVar "b"]
            (PType $ typevar "a" :-> (typevar "b" :-> typevar "a"))
        )
    runTypeChecker
      []
      (mkLambda defPos "x" (mkLambda defPos "y" (mkAdd defPos (mkVariable defPos "x") (mkVariable defPos "y"))))
      `shouldBe` Right (PType (int :-> (int :-> int)))
  it "rejects ill-typed expressions" testErrors
  it "updates TypingEnv" $ do
    runTypeChecker' [] (mkDefinition defPos "a" (litInt defPos 1))
      `shouldBe` Right (PType int, [("a", PType int)])

testErrors :: Expectation
testErrors = do
  runTypeChecker [] (mkLambda defPos "x" (mkApplication defPos (mkVariable defPos "x") (mkVariable defPos "x")))
    `shouldBe` Left
      ( InfiniteTypeError
          (TypeVar "a1")
          (typevar "a1" :-> typevar "a2")
      )
  runTypeChecker
    []
    (mkApplication defPos (mkLambda defPos "x" (mkAdd defPos (mkVariable defPos "x") (litBool defPos True))) (litInt defPos 2))
    `shouldBe` Left (MismatchedTypesError int bool)
