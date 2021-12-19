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
    it "parses module declarations" testParseModules
    it "parses math expressions"    testParseMath
    it "parses variables"           testParseVariables
    it "parses lambdas"             testParseLambdas
    it "parses definitions"         testParseDefinitions
--    it "parses applications"     testParseApplications

testParseModules :: Expectation
testParseModules = do
  runParser parseModule "module TestModule where"
    `shouldBe` Module "TestModule" []

testParseMath :: Expectation
testParseMath = do
  runParser (parseOperator emptyEnv (Literal (LitInt 1))) "+1"
    `shouldBe` Add (Literal (LitInt 1)) (Literal (LitInt 1))
  runParser (parseOperator emptyEnv (Literal (LitInt 1))) "-1"
    `shouldBe` Sub (Literal (LitInt 1)) (Literal (LitInt 1))
  runParser (parseOperator emptyEnv (Literal (LitInt 1))) "*1"
    `shouldBe` Mul (Literal (LitInt 1)) (Literal (LitInt 1))
  runParser (parseOperator emptyEnv (Literal (LitInt 1))) "/1"
    `shouldBe` Div (Literal (LitInt 1)) (Literal (LitInt 1))
  runParser (parseOperator emptyEnv (Variable "a")) "+b"
    `shouldBe` Add (Variable "a") (Variable "b")
  runParser (parseOperator emptyEnv (Variable "a")) "-b"
    `shouldBe` Sub (Variable "a") (Variable "b")
  runParser (parseOperator emptyEnv (Variable "a")) "*b"
    `shouldBe` Mul (Variable "a") (Variable "b")
  runParser (parseOperator emptyEnv (Variable "a")) "/b"
    `shouldBe` Div (Variable "a") (Variable "b")
  runParser (parseExpr emptyEnv) "1+1"
    `shouldBe` Add (Literal (LitInt 1)) (Literal (LitInt 1))
  runParser (parseExpr emptyEnv) "1-1"
    `shouldBe` Sub (Literal (LitInt 1)) (Literal (LitInt 1))
  runParser (parseExpr emptyEnv) "1*1"
    `shouldBe` Mul (Literal (LitInt 1)) (Literal (LitInt 1))
  runParser (parseExpr emptyEnv) "1/1"
    `shouldBe` Div (Literal (LitInt 1)) (Literal (LitInt 1))
  runParser (parseExpr emptyEnv) "a+b"
    `shouldBe` Add (Variable "a") (Variable "b")
  runParser (parseExpr emptyEnv) "a-b"
    `shouldBe` Sub (Variable "a") (Variable "b")
  runParser (parseExpr emptyEnv) "a*b"
    `shouldBe` Mul (Variable "a") (Variable "b")
  runParser (parseExpr emptyEnv) "a/b"
    `shouldBe` Div (Variable "a") (Variable "b")
  runParser (parseExpr emptyEnv) "1*2*3+4"
    `shouldBe` Add (Mul (Literal (LitInt 1)) (Mul (Literal (LitInt 2)) (Literal (LitInt 3)))) (Literal (LitInt 4))

testParseVariables :: Expectation
testParseVariables = do
  runParser parseVariable "xyz" `shouldBe` Variable "xyz"

testParseLambdas :: Expectation
testParseLambdas = do
  map (runParser (parseLambda emptyEnv))
      ["λa.a", "λ a.a", "λ a .a", "λa. a", "λ a . a"]
    `shouldSatisfy` all (== Lambda "a" (Variable "a"))
  runParser (parseLambda emptyEnv) "λa.λb.a"
    `shouldBe` Lambda "a" (Lambda "b" (Variable "a"))
  runParser (parseLambda emptyEnv) "λa. λb. a"
    `shouldBe` Lambda "a" (Lambda "b" (Variable "a"))

testParseDefinitions :: Expectation
testParseDefinitions = do
  map (runParser (parseDef emptyEnv))
      ["id = λa.a", "id=λa.a", "id =λa.a", "id= λa.a"]
    `shouldSatisfy` all (== Definition "id" (Lambda "a" (Variable "a")))
  runParser (parseDef emptyEnv) "v = 42"
    `shouldBe` Definition "v" (Literal (LitInt 42))

testParseApplications :: Expectation
testParseApplications = do
  runParser
      (parseApp emptyEnv (Lambda "x" (Add (Variable "x") (Literal (LitInt 1)))))
      " 2"
    `shouldBe` Application
                 (Lambda "x" (Add (Variable "x") (Literal (LitInt 1))))
                 (Literal (LitInt 2))
  runParser (parseExpr emptyEnv) "λx.x+1 2" `shouldBe` Application
    (Lambda "x" (Add (Variable "x") (Literal (LitInt 1))))
    (Literal (LitInt 2))

emptyEnv :: Env
emptyEnv = Env { operators = [], operands = [] }
