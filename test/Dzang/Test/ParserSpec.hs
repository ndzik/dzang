module Dzang.Test.ParserSpec where

import           Data.Foldable
import           Dzang.Language
import           Dzarser.Parser
import           Test.Hspec                     ( Spec
                                                , SpecWith
                                                , describe
                                                , it
                                                )
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "Dzang Parser" $ do
    it "parses module declarations" testParseModules
    it "parses variables"           testParseVariables
    it "parses lambdas" testParseLambdas
    it "parses definitions"         testParseDefinitions
    it "parses applications"        testParseApplications
    it "parses brackets"            testParseBrackets

    describe "math expressions" testParseMath

testParseModules :: Expectation
testParseModules = do
  runParser parseModule "module TestModule where"
    `shouldBe` Module "TestModule" []

testParseMath :: SpecWith ()
testParseMath = do
  it "parses basic arithmetic"            testBasicArithmetic
  it "parses right associative operators" testRightAssociativeOperators
  it "parses left associative operators"  testLeftAssociativeOperators
  it "parses complex nested arithmetic"   testComplexMath

testRightAssociativeOperators :: Expectation
testRightAssociativeOperators = do
  traverse_
    (\(input, expec) -> runParser (parseExpr emptyEnv) input `shouldBe` expec)
    [ ( "1+2+3+4"
        , Add
          (Add
            (Add (Literal (LitInt 1)) (Literal (LitInt 2)))
            (Literal (LitInt 3)))
          (Literal (LitInt 4))
      )
    , ( "1*2*3*4"
        , Mul
          (Mul
            (Mul (Literal (LitInt 1)) (Literal (LitInt 2)))
            (Literal (LitInt 3)))
          (Literal (LitInt 4))
      )
    ]

testLeftAssociativeOperators :: Expectation
testLeftAssociativeOperators = do
  traverse_
    (\(input, expec) -> runParser (parseExpr emptyEnv) input `shouldBe` expec)
    [ ( "1-2-3-4"
      , Sub
        (Sub (Sub (Literal (LitInt 1)) (Literal (LitInt 2)))
             (Literal (LitInt 3))
        )
        (Literal (LitInt 4))
      )
    ]

testComplexMath :: Expectation
testComplexMath = do
  traverse_
    (\(input, expec) -> runParser (parseExpr emptyEnv) input `shouldBe` expec)
    [ ( "2*3-4+5-6+x"
      , Add
        (Sub
          (Add
            (Sub (Mul (Literal (LitInt 2)) (Literal (LitInt 3)))
                 (Literal (LitInt 4))
            )

            (Literal (LitInt 5))
          )
          (Literal (LitInt 6))
        )
        (Variable "x")
      )
    , ( "λx.2*3-4+5-6+x 10"
      , Application
        (Lambda
          "x"
          (Add
            (Sub
              (Add
                (Sub (Mul (Literal (LitInt 2)) (Literal (LitInt 3)))
                     (Literal (LitInt 4))
                )

                (Literal (LitInt 5))
              )
              (Literal (LitInt 6))
            )
            (Variable "x")
          )
        )
        (Literal (LitInt 10))
      )
    ]

testBasicArithmetic :: Expectation
testBasicArithmetic = do
  runParser (parseOperator '+' emptyEnv{ operands = [Literal (LitInt 1)] }) "+1"
    `shouldBe` Add (Literal (LitInt 1)) (Literal (LitInt 1))
  runParser (parseOperator '-' emptyEnv{ operands = [Literal (LitInt 1)] }) "-1"
    `shouldBe` Sub (Literal (LitInt 1)) (Literal (LitInt 1))
  runParser (parseOperator '*' emptyEnv{ operands = [Literal (LitInt 1)] }) "*1"
    `shouldBe` Mul (Literal (LitInt 1)) (Literal (LitInt 1))
  runParser (parseOperator '/' emptyEnv{ operands = [Literal (LitInt 1)] }) "/1"
    `shouldBe` Div (Literal (LitInt 1)) (Literal (LitInt 1))
  runParser (parseOperator '+' emptyEnv{ operands = [Variable "a"] }) "+b"
    `shouldBe` Add (Variable "a") (Variable "b")
  runParser (parseOperator '-' emptyEnv{ operands = [Variable "a"] }) "-b"
    `shouldBe` Sub (Variable "a") (Variable "b")
  runParser (parseOperator '*' emptyEnv{ operands = [Variable "a"] }) "*b"
    `shouldBe` Mul (Variable "a") (Variable "b")
  runParser (parseOperator '/' emptyEnv{ operands = [Variable "a"] }) "/b"
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
  runParser (parseExpr emptyEnv) "1*2*3+4" `shouldBe` Add
    (Mul (Mul (Literal (LitInt 1)) (Literal (LitInt 2))) (Literal (LitInt 3)))
    (Literal (LitInt 4))

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

testParseBrackets :: Expectation
testParseBrackets = do
  runParser (parseBracket emptyEnv) "(a+a)"
    `shouldBe` Add (Variable "a") (Variable "a")
  traverse_
    (\(input, expec) -> runParser (parseExpr emptyEnv) input `shouldBe` expec)
    [ ("(a+a)*a", Mul (Add (Variable "a") (Variable "a")) (Variable "a"))
    , ( "(λa.λb.a) 42"
      , Application (Lambda "a" (Lambda "b" (Variable "a")))
                    (Literal (LitInt 42))
      )
    , ( "(λa.λb.a) 42 69"
      , Application
        (Application (Lambda "a" (Lambda "b" (Variable "a")))
                     (Literal (LitInt 42))
        )
        (Literal (LitInt 69))
      )
    ]
