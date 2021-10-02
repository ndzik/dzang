module Dzang.Test.ParserSpec where

import           Dzang.Parser
import           Test.Hspec

spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses numbers" $ do
      runParser number "1234" `shouldBe` 1234
    it "reports errors" $ do
      runParser (number >> separator ';' >> number) "1234,1234" `shouldBe` (ParseResult [ParseError UnexpectedToken (Line 1) (Col 5)])
