module Dzang.Test.ParserSpec where

import Data.Foldable
import Dzang.AST
import Dzang.Language
import Dzarser.Stateful.Parser
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
  evalParser parseModule "module TestModule where"
    `shouldBe` Right (mkModule "TestModule" [])

testParseMath :: SpecWith ()
testParseMath = do
  it "parses basic arithmetic" testBasicArithmetic
  it "parses right associative operators" testRightAssociativeOperators
  it "parses left associative operators" testLeftAssociativeOperators
  it "parses complex nested arithmetic" testComplexMath

testRightAssociativeOperators :: Expectation
testRightAssociativeOperators = do
  traverse_
    (\(input, expec) -> evalParser (parseExpr emptyEnv) input `shouldBe` expec)
    [ ("1+2+3+4", Right (mkAdd (mkAdd (mkAdd (litInt 1) (litInt 2)) (litInt 3)) (litInt 4))),
      ("1*2*3*4", Right (mkMul (mkMul (mkMul (litInt 1) (litInt 2)) (litInt 3)) (litInt 4)))
    ]

testLeftAssociativeOperators :: Expectation
testLeftAssociativeOperators = do
  traverse_
    (\(input, expec) -> evalParser (parseExpr emptyEnv) input `shouldBe` expec)
    [("1-2-3-4", Right (mkSub (mkSub (mkSub (litInt 1) (litInt 2)) (litInt 3)) (litInt 4)))]

testComplexMath :: Expectation
testComplexMath = do
  traverse_
    (\(input, expec) -> evalParser (parseExpr emptyEnv) input `shouldBe` expec)
    [ ( "2*3-4+5-6+x",
        Right
          ( mkAdd
              ( mkSub
                  (mkAdd (mkSub (mkMul (litInt 2) (litInt 3)) (litInt 4)) (litInt 5))
                  (litInt 6)
              )
              (mkVariable "x")
          )
      ),
      ( "λx.2*3-4+5-6+x 10",
        Right
          ( mkApplication
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
      )
    ]

testBasicArithmetic :: Expectation
testBasicArithmetic = do
  evalParser (parseOperator '+' emptyEnv {operands = [litInt 1]}) "+1"
    `shouldBe` Right (mkAdd (litInt 1) (litInt 1))
  evalParser (parseOperator '-' emptyEnv {operands = [litInt 1]}) "-1"
    `shouldBe` Right (mkSub (litInt 1) (litInt 1))
  evalParser (parseOperator '*' emptyEnv {operands = [litInt 1]}) "*1"
    `shouldBe` Right (mkMul (litInt 1) (litInt 1))
  evalParser (parseOperator '/' emptyEnv {operands = [litInt 1]}) "/1"
    `shouldBe` Right (mkDiv (litInt 1) (litInt 1))
  evalParser (parseOperator '+' emptyEnv {operands = [mkVariable "a"]}) "+b"
    `shouldBe` Right (mkAdd (mkVariable "a") (mkVariable "b"))
  evalParser (parseOperator '-' emptyEnv {operands = [mkVariable "a"]}) "-b"
    `shouldBe` Right (mkSub (mkVariable "a") (mkVariable "b"))
  evalParser (parseOperator '*' emptyEnv {operands = [mkVariable "a"]}) "*b"
    `shouldBe` Right (mkMul (mkVariable "a") (mkVariable "b"))
  evalParser (parseOperator '/' emptyEnv {operands = [mkVariable "a"]}) "/b"
    `shouldBe` Right (mkDiv (mkVariable "a") (mkVariable "b"))
  evalParser (parseExpr emptyEnv) "1+1" `shouldBe` Right (mkAdd (litInt 1) (litInt 1))
  evalParser (parseExpr emptyEnv) "1-1" `shouldBe` Right (mkSub (litInt 1) (litInt 1))
  evalParser (parseExpr emptyEnv) "1*1" `shouldBe` Right (mkMul (litInt 1) (litInt 1))
  evalParser (parseExpr emptyEnv) "1/1" `shouldBe` Right (mkDiv (litInt 1) (litInt 1))
  evalParser (parseExpr emptyEnv) "a+b"
    `shouldBe` Right (mkAdd (mkVariable "a") (mkVariable "b"))
  evalParser (parseExpr emptyEnv) "a-b"
    `shouldBe` Right (mkSub (mkVariable "a") (mkVariable "b"))
  evalParser (parseExpr emptyEnv) "a*b"
    `shouldBe` Right (mkMul (mkVariable "a") (mkVariable "b"))
  evalParser (parseExpr emptyEnv) "a/b"
    `shouldBe` Right (mkDiv (mkVariable "a") (mkVariable "b"))
  evalParser (parseExpr emptyEnv) "1*2*3+4"
    `shouldBe` Right (mkAdd (mkMul (mkMul (litInt 1) (litInt 2)) (litInt 3)) (litInt 4))

testParseVariables :: Expectation
testParseVariables = do
  evalParser parseVariable "xyz" `shouldBe` Right (mkVariable "xyz")

testParseLambdas :: Expectation
testParseLambdas = do
  map
    (evalParser (parseLambda emptyEnv))
    ["λa.a", "λ a.a", "λ a .a", "λa. a", "λ a . a"]
    `shouldSatisfy` all (== Right (mkLambda "a" (mkVariable "a")))
  evalParser (parseLambda emptyEnv) "λa.λb.a"
    `shouldBe` Right (mkLambda "a" (mkLambda "b" (mkVariable "a")))
  evalParser (parseLambda emptyEnv) "λa. λb. a"
    `shouldBe` Right (mkLambda "a" (mkLambda "b" (mkVariable "a")))

testParseDefinitions :: Expectation
testParseDefinitions = do
  map
    (evalParser (parseDef emptyEnv))
    ["id = λa.a", "id=λa.a", "id =λa.a", "id= λa.a"]
    `shouldSatisfy` all (== Right (mkDefinition "id" (mkLambda "a" (mkVariable "a"))))
  evalParser (parseDef emptyEnv) "v = 42" `shouldBe` Right (mkDefinition "v" (litInt 42))
  evalParser (parseDef emptyEnv) "v=1+2"
    `shouldBe` Right (mkDefinition "v" (mkAdd (litInt 1) (litInt 2)))

testParseApplications :: Expectation
testParseApplications = do
  evalParser
    (parseApp emptyEnv (mkLambda "x" (mkAdd (mkVariable "x") (litInt 1))))
    " 2"
    `shouldBe` Right
      ( mkApplication
          (mkLambda "x" (mkAdd (mkVariable "x") (litInt 1)))
          (litInt 2)
      )
  evalParser (parseExpr emptyEnv) "λx.x+1 2"
    `shouldBe` Right
      ( mkApplication
          (mkLambda "x" (mkAdd (mkVariable "x") (litInt 1)))
          (litInt 2)
      )
  evalParser (parseExpr emptyEnv) "λf.λx.λy.(f x y)"
    `shouldBe` Right
      ( mkLambda
          "f"
          ( mkLambda
              "x"
              ( mkLambda
                  "y"
                  (mkApplication (mkApplication (mkVariable "f") (mkVariable "x")) (mkVariable "y"))
              )
          )
      )

testParseBrackets :: Expectation
testParseBrackets = do
  evalParser (parseBracket emptyEnv) "(a+a)"
    `shouldBe` Right (mkAdd (mkVariable "a") (mkVariable "a"))
  traverse_
    (\(input, expec) -> evalParser (parseExpr emptyEnv) input `shouldBe` expec)
    [ ("(a+a)*a", Right (mkMul (mkAdd (mkVariable "a") (mkVariable "a")) (mkVariable "a"))),
      ( "(λa.λb.a) 42",
        Right (mkApplication (mkLambda "a" (mkLambda "b" (mkVariable "a"))) (litInt 42))
      ),
      ( "(λa.λb.a) 42 69",
        Right
          ( mkApplication
              (mkApplication (mkLambda "a" (mkLambda "b" (mkVariable "a"))) (litInt 42))
              (litInt 69)
          )
      ),
      ( "f 42 69",
        Right (mkApplication (mkApplication (mkVariable "f") (litInt 42)) (litInt 69))
      )
    ]
