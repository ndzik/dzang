module Dzang.Test.TypeCheckerSpec where

import           Dzang.TypeChecker
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                )
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "Dzang Typechecker" $ do
    it "types valid expressions" $ do
      runChecker "1+1" `shouldBe` TInt
      runChecker "λx.x" `shouldBe` TArr "x" (TPoly "x") (TPoly "x")
      runChecker "λx.1+1" `shouldBe` TArr "x" (TPoly "x") TInt
      runChecker "λx.x+1" `shouldBe` TArr "x" TInt TInt
--      runChecker "λx.λy.x+y"
--        `shouldBe` TArr "x" (TPoly "x") (TArr "y" (TPoly "x") (TPoly "y"))
