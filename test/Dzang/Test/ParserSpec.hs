module Dzang.Test.ParserSpec where

import Data.Foldable
import Dzang.Language
import Dzang.AST
import Dzarser.Parser
import Test.Hspec
  ( Spec,
    SpecWith,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "Dzang Parser" $ do
    it "parses module declarations" testParseModules
    it "parses variables" testParseVariables
    it "parses lambdas" testParseLambdas
    it "parses definitions" testParseDefinitions
    it "parses applications" testParseApplications
    it "parses brackets" testParseBrackets

    describe "math expressions" testParseMath

testParseModules :: Expectation
testParseModules = do
  runParser parseModule "module TestModule where"
    `shouldBe` mkModule "TestModule" []

testParseMath :: SpecWith ()
testParseMath = do
  it "parses basic arithmetic" testBasicArithmetic
  it "parses right associative operators" testRightAssociativeOperators
  it "parses left associative operators" testLeftAssociativeOperators
  it "parses complex nested arithmetic" testComplexMath

testRightAssociativeOperators :: Expectation
testRightAssociativeOperators = do
  traverse_
    (\(input, expec) -> runParser (parseExpr emptyEnv) input `shouldBe` expec)
    [ ("1+2+3+4", mkAdd (mkAdd (mkAdd (litInt 1) (litInt 2)) (litInt 3)) (litInt 4)),
      ("1*2*3*4", mkMul (mkMul (mkMul (litInt 1) (litInt 2)) (litInt 3)) (litInt 4))
    ]

testLeftAssociativeOperators :: Expectation
testLeftAssociativeOperators = do
  traverse_
    (\(input, expec) -> runParser (parseExpr emptyEnv) input `shouldBe` expec)
    [("1-2-3-4", mkSub (mkSub (mkSub (litInt 1) (litInt 2)) (litInt 3)) (litInt 4))]

testComplexMath :: Expectation
testComplexMath = do
  traverse_
    (\(input, expec) -> runParser (parseExpr emptyEnv) input `shouldBe` expec)
    [ ( "2*3-4+5-6+x",
        mkAdd
          ( mkSub
              (mkAdd (mkSub (mkMul (litInt 2) (litInt 3)) (litInt 4)) (litInt 5))
              (litInt 6)
          )
          (mkVariable "x")
      ),
      ( "λx.2*3-4+5-6+x 10",
        mkApplication
          ( mkLambda
              "x"
              ( mkAdd
                  ( mkSub
                      (mkAdd (mkSub (mkMul (litInt 2) (litInt 3)) (litInt 4)) (litInt 5))
                      (litInt 6)
                  )
                  (mkVariable "x")
              )
          )
          (litInt 10)
      )
    ]

testBasicArithmetic :: Expectation
testBasicArithmetic = do
  runParser (parseOperator '+' emptyEnv {operands = [litInt 1]}) "+1"
    `shouldBe` mkAdd (litInt 1) (litInt 1)
  runParser (parseOperator '-' emptyEnv {operands = [litInt 1]}) "-1"
    `shouldBe` mkSub (litInt 1) (litInt 1)
  runParser (parseOperator '*' emptyEnv {operands = [litInt 1]}) "*1"
    `shouldBe` mkMul (litInt 1) (litInt 1)
  runParser (parseOperator '/' emptyEnv {operands = [litInt 1]}) "/1"
    `shouldBe` mkDiv (litInt 1) (litInt 1)
  runParser (parseOperator '+' emptyEnv {operands = [mkVariable "a"]}) "+b"
    `shouldBe` mkAdd (mkVariable "a") (mkVariable "b")
  runParser (parseOperator '-' emptyEnv {operands = [mkVariable "a"]}) "-b"
    `shouldBe` mkSub (mkVariable "a") (mkVariable "b")
  runParser (parseOperator '*' emptyEnv {operands = [mkVariable "a"]}) "*b"
    `shouldBe` mkMul (mkVariable "a") (mkVariable "b")
  runParser (parseOperator '/' emptyEnv {operands = [mkVariable "a"]}) "/b"
    `shouldBe` mkDiv (mkVariable "a") (mkVariable "b")
  runParser (parseExpr emptyEnv) "1+1" `shouldBe` mkAdd (litInt 1) (litInt 1)
  runParser (parseExpr emptyEnv) "1-1" `shouldBe` mkSub (litInt 1) (litInt 1)
  runParser (parseExpr emptyEnv) "1*1" `shouldBe` mkMul (litInt 1) (litInt 1)
  runParser (parseExpr emptyEnv) "1/1" `shouldBe` mkDiv (litInt 1) (litInt 1)
  runParser (parseExpr emptyEnv) "a+b"
    `shouldBe` mkAdd (mkVariable "a") (mkVariable "b")
  runParser (parseExpr emptyEnv) "a-b"
    `shouldBe` mkSub (mkVariable "a") (mkVariable "b")
  runParser (parseExpr emptyEnv) "a*b"
    `shouldBe` mkMul (mkVariable "a") (mkVariable "b")
  runParser (parseExpr emptyEnv) "a/b"
    `shouldBe` mkDiv (mkVariable "a") (mkVariable "b")
  runParser (parseExpr emptyEnv) "1*2*3+4"
    `shouldBe` mkAdd (mkMul (mkMul (litInt 1) (litInt 2)) (litInt 3)) (litInt 4)

testParseVariables :: Expectation
testParseVariables = do
  runParser parseVariable "xyz" `shouldBe` mkVariable "xyz"

testParseLambdas :: Expectation
testParseLambdas = do
  map
    (runParser (parseLambda emptyEnv))
    ["λa.a", "λ a.a", "λ a .a", "λa. a", "λ a . a"]
    `shouldSatisfy` all (== mkLambda "a" (mkVariable "a"))
  runParser (parseLambda emptyEnv) "λa.λb.a"
    `shouldBe` mkLambda "a" (mkLambda "b" (mkVariable "a"))
  runParser (parseLambda emptyEnv) "λa. λb. a"
    `shouldBe` mkLambda "a" (mkLambda "b" (mkVariable "a"))

testParseDefinitions :: Expectation
testParseDefinitions = do
  map
    (runParser (parseDef emptyEnv))
    ["id = λa.a", "id=λa.a", "id =λa.a", "id= λa.a"]
    `shouldSatisfy` all (== mkDefinition "id" (mkLambda "a" (mkVariable "a")))
  runParser (parseDef emptyEnv) "v = 42" `shouldBe` mkDefinition "v" (litInt 42)
  runParser (parseDef emptyEnv) "v=1+2"
    `shouldBe` mkDefinition "v" (mkAdd (litInt 1) (litInt 2))

testParseApplications :: Expectation
testParseApplications = do
  runParser
    (parseApp emptyEnv (mkLambda "x" (mkAdd (mkVariable "x") (litInt 1))))
    " 2"
    `shouldBe` mkApplication
      (mkLambda "x" (mkAdd (mkVariable "x") (litInt 1)))
      (litInt 2)
  runParser (parseExpr emptyEnv) "λx.x+1 2"
    `shouldBe` mkApplication
      (mkLambda "x" (mkAdd (mkVariable "x") (litInt 1)))
      (litInt 2)
  runParser (parseExpr emptyEnv) "λf.λx.λy.(f x y)"
    `shouldBe` mkLambda
      "f"
      ( mkLambda
          "x"
          ( mkLambda
              "y"
              (mkApplication (mkApplication (mkVariable "f") (mkVariable "x")) (mkVariable "y"))
          )
      )

testParseBrackets :: Expectation
testParseBrackets = do
  runParser (parseBracket emptyEnv) "(a+a)"
    `shouldBe` mkAdd (mkVariable "a") (mkVariable "a")
  traverse_
    (\(input, expec) -> runParser (parseExpr emptyEnv) input `shouldBe` expec)
    [ ("(a+a)*a", mkMul (mkAdd (mkVariable "a") (mkVariable "a")) (mkVariable "a")),
      ( "(λa.λb.a) 42",
        mkApplication (mkLambda "a" (mkLambda "b" (mkVariable "a"))) (litInt 42)
      ),
      ( "(λa.λb.a) 42 69",
        mkApplication
          (mkApplication (mkLambda "a" (mkLambda "b" (mkVariable "a"))) (litInt 42))
          (litInt 69)
      ),
      ( "f 42 69",
        mkApplication (mkApplication (mkVariable "f") (litInt 42)) (litInt 69)
      )
    ]
