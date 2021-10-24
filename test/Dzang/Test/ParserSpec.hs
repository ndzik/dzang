module Dzang.Test.ParserSpec where

import           Dzang.Language
import           Dzarser.Parser
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                )
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "Dzang Parser" $ do
    it "requires modules to start with module declaration" $ do
      runParser parseModule "module TestModule where"
        `shouldBe` Module "TestModule" []
    it "parses lambdas" $ do
      map (runParser parseLambda)
          ["λa.a", "λ a.a", "λ a .a", "λa. a", "λ a . a"]
        `shouldSatisfy` all (== Lambda "a" (Variable "a"))
      runParser parseLambda "λa.λb.a"
        `shouldBe` Lambda "a" (Lambda "b" (Variable "a"))
      runParser parseLambda "λa. λb. a"
        `shouldBe` Lambda "a" (Lambda "b" (Variable "a"))
    it "parses definitions" $ do
      map (runParser parseDef) ["id = λa.a", "id=λa.a", "id =λa.a", "id= λa.a"]
        `shouldSatisfy` all (== Definition "id" (Lambda "a" (Variable "a")))
      runParser parseDef "v = 42"
        `shouldBe` Definition "v" (Literal (LitInt 42))
