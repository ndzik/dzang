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

defPos :: (Int, Int)
defPos = (1, 1)

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
    [ ("1+2+3+4", Right (mkAdd (1, 6) (mkAdd (1, 4) (mkAdd (1, 2) (litInt (1, 1) 1) (litInt (1, 3) 2)) (litInt (1, 5) 3)) (litInt (1, 7) 4))),
      ("1*2*3*4", Right (mkMul (1, 6) (mkMul (1, 4) (mkMul (1, 2) (litInt (1, 1) 1) (litInt (1, 3) 2)) (litInt (1, 5) 3)) (litInt (1, 7) 4)))
    ]

testLeftAssociativeOperators :: Expectation
testLeftAssociativeOperators = do
  traverse_
    (\(input, expec) -> evalParser (parseExpr emptyEnv) input `shouldBe` expec)
    [("1-2-3-4", Right (mkSub (1, 6) (mkSub (1, 4) (mkSub (1, 2) (litInt (1, 1) 1) (litInt (1, 3) 2)) (litInt (1, 5) 3)) (litInt (1, 7) 4)))]

testComplexMath :: Expectation
testComplexMath = do
  traverse_
    (\(input, expec) -> evalParser (parseExpr emptyEnv) input `shouldBe` expec)
    [ ( "2*3-4+5-6+x",
        Right
          ( mkAdd
              (1, 10)
              ( mkSub
                  (1, 8)
                  (mkAdd (1, 6) (mkSub (1, 4) (mkMul (1, 2) (litInt (1, 1) 2) (litInt (1, 3) 3)) (litInt (1, 5) 4)) (litInt (1, 7) 5))
                  (litInt (1, 9) 6)
              )
              (mkVariable (1, 11) "x")
          )
      ),
      ( "λx.2*3-4+5-6+x 10",
        Right
          ( mkApplication
              (1, 15)
              ( mkLambda
                  (1, 1)
                  "x"
                  ( mkAdd
                      (1, 13)
                      ( mkSub
                          (1, 11)
                          (mkAdd (1, 9) (mkSub (1, 7) (mkMul (1, 5) (litInt (1, 4) 2) (litInt (1, 6) 3)) (litInt (1, 8) 4)) (litInt (1, 10) 5))
                          (litInt (1, 12) 6)
                      )
                      (mkVariable (1, 14) "x")
                  )
              )
              (litInt (1, 16) 10)
          )
      )
    ]

pP :: Int -> Int -> ParserState
pP = ParserState

testBasicArithmetic :: Expectation
testBasicArithmetic = do
  evalParser' (parseOperator '+' emptyEnv {operands = [litInt (1, 1) 1]}) (pP 1 2) "+1"
    `shouldBe` Right (mkAdd (1, 2) (litInt (1, 1) 1) (litInt (1, 3) 1))
  evalParser' (parseOperator '-' emptyEnv {operands = [litInt defPos 1]}) (pP 1 2) "-1"
    `shouldBe` Right (mkSub (1, 2) (litInt defPos 1) (litInt (1, 3) 1))
  evalParser' (parseOperator '*' emptyEnv {operands = [litInt defPos 1]}) (pP 1 2) "*1"
    `shouldBe` Right (mkMul (1, 2) (litInt defPos 1) (litInt (1, 3) 1))
  evalParser' (parseOperator '/' emptyEnv {operands = [litInt defPos 1]}) (pP 1 2) "/1"
    `shouldBe` Right (mkDiv (1, 2) (litInt defPos 1) (litInt (1, 3) 1))

  evalParser' (parseOperator '+' emptyEnv {operands = [mkVariable (1, 1) "a"]}) (pP 1 2) "+b"
    `shouldBe` Right (mkAdd (1, 2) (mkVariable defPos "a") (mkVariable (1, 3) "b"))
  evalParser' (parseOperator '-' emptyEnv {operands = [mkVariable (1, 1) "a"]}) (pP 1 2) "-b"
    `shouldBe` Right (mkSub (1, 2) (mkVariable defPos "a") (mkVariable (1, 3) "b"))
  evalParser' (parseOperator '*' emptyEnv {operands = [mkVariable (1, 1) "a"]}) (pP 1 2) "*b"
    `shouldBe` Right (mkMul (1, 2) (mkVariable defPos "a") (mkVariable (1, 3) "b"))
  evalParser' (parseOperator '/' emptyEnv {operands = [mkVariable (1, 1) "a"]}) (pP 1 2) "/b"
    `shouldBe` Right (mkDiv (1, 2) (mkVariable defPos "a") (mkVariable (1, 3) "b"))

  evalParser' (parseExpr emptyEnv) (pP 1 1) "1+1" `shouldBe` Right (mkAdd (1, 2) (litInt (1, 1) 1) (litInt (1, 3) 1))
  evalParser' (parseExpr emptyEnv) (pP 1 1) "1-1" `shouldBe` Right (mkSub (1, 2) (litInt (1, 1) 1) (litInt (1, 3) 1))
  evalParser' (parseExpr emptyEnv) (pP 1 1) "1*1" `shouldBe` Right (mkMul (1, 2) (litInt (1, 1) 1) (litInt (1, 3) 1))
  evalParser' (parseExpr emptyEnv) (pP 1 1) "1/1" `shouldBe` Right (mkDiv (1, 2) (litInt (1, 1) 1) (litInt (1, 3) 1))

  evalParser' (parseExpr emptyEnv) (pP 1 1) "a+b"
    `shouldBe` Right (mkAdd (1, 2) (mkVariable defPos "a") (mkVariable (1, 3) "b"))
  evalParser' (parseExpr emptyEnv) (pP 1 1) "a-b"
    `shouldBe` Right (mkSub (1, 2) (mkVariable defPos "a") (mkVariable (1, 3) "b"))
  evalParser' (parseExpr emptyEnv) (pP 1 1) "a*b"
    `shouldBe` Right (mkMul (1, 2) (mkVariable defPos "a") (mkVariable (1, 3) "b"))
  evalParser' (parseExpr emptyEnv) (pP 1 1) "a/b"
    `shouldBe` Right (mkDiv (1, 2) (mkVariable defPos "a") (mkVariable (1, 3) "b"))

  evalParser' (parseExpr emptyEnv) (pP 1 1) "1*2*3+4"
    `shouldBe` Right (mkAdd (1, 6) (mkMul (1, 4) (mkMul (1, 2) (litInt (1, 1) 1) (litInt (1, 3) 2)) (litInt (1, 5) 3)) (litInt (1, 7) 4))

testParseVariables :: Expectation
testParseVariables = do
  evalParser parseVariable "xyz" `shouldBe` Right (mkVariable defPos "xyz")

testParseLambdas :: Expectation
testParseLambdas = do
  traverse_
    ( \(s, (p1, p2)) ->
        evalParser (parseLambda emptyEnv) s
          `shouldBe` Right (mkLambda p1 "a" (mkVariable p2 "a"))
    )
    [("λa.a", ((1, 1), (1, 4))), ("λ a.a", ((1, 1), (1, 5))), ("λ a .a", ((1, 1), (1, 6))), ("λa. a", ((1, 1), (1, 5))), ("λ a . a", ((1, 1), (1, 7)))]

  evalParser (parseLambda emptyEnv) "λa.λb.a"
    `shouldBe` Right (mkLambda (1, 1) "a" (mkLambda (1, 4) "b" (mkVariable (1, 7) "a")))
  evalParser (parseLambda emptyEnv) "λa. λb. a"
    `shouldBe` Right (mkLambda (1, 1) "a" (mkLambda (1, 5) "b" (mkVariable (1, 9) "a")))

testParseDefinitions :: Expectation
testParseDefinitions = do
  traverse_
    (\(s, (p1, p2, p3)) -> evalParser (parseDef emptyEnv) s `shouldBe` Right (mkDefinition p1 "id" (mkLambda p2 "a" (mkVariable p3 "a"))))
    [("id = λa.a", ((1, 1), (1, 6), (1, 9))), ("id=λa.a", ((1, 1), (1, 4), (1, 7))), ("id =λa.a", ((1, 1), (1, 5), (1, 8))), ("id= λa.a", ((1, 1), (1, 5), (1, 8)))]
  evalParser (parseDef emptyEnv) "v = 42" `shouldBe` Right (mkDefinition (1, 1) "v" (litInt (1, 5) 42))
  evalParser (parseDef emptyEnv) "v=1+2"
    `shouldBe` Right (mkDefinition (1, 1) "v" (mkAdd (1, 4) (litInt (1, 3) 1) (litInt (1, 5) 2)))

testParseApplications :: Expectation
testParseApplications = do
  evalParser'
    (parseApp emptyEnv (mkLambda (1, 1) "x" (mkAdd (1, 5) (mkVariable (1, 4) "x") (litInt (1, 6) 1))))
    (pP 1 7)
    " 2"
    `shouldBe` Right
      ( mkApplication
          (1, 7)
          (mkLambda (1, 1) "x" (mkAdd (1, 5) (mkVariable (1, 4) "x") (litInt (1, 6) 1)))
          (litInt (1, 8) 2)
      )

  evalParser (parseExpr emptyEnv) "λx.x+1 2"
    `shouldBe` Right
      ( mkApplication
          (1, 7)
          (mkLambda (1, 1) "x" (mkAdd (1, 5) (mkVariable (1, 4) "x") (litInt (1, 6) 1)))
          (litInt (1, 8) 2)
      )

  evalParser (parseExpr emptyEnv) "λf.λx.λy.(f x y)"
    `shouldBe` Right
      ( mkLambda
          (1, 1)
          "f"
          ( mkLambda
              (1, 4)
              "x"
              ( mkLambda
                  (1, 7)
                  "y"
                  (mkApplication (1, 14) (mkApplication (1, 12) (mkVariable (1, 11) "f") (mkVariable (1, 13) "x")) (mkVariable (1, 15) "y"))
              )
          )
      )

testParseBrackets :: Expectation
testParseBrackets = do
  evalParser (parseBracket emptyEnv) "(a+a)"
    `shouldBe` Right (mkAdd (1, 3) (mkVariable (1, 2) "a") (mkVariable (1, 4) "a"))
  traverse_
    (\(input, expec) -> evalParser (parseExpr emptyEnv) input `shouldBe` expec)
    [ ("(a+a)*a", Right (mkMul (1, 6) (mkAdd (1, 3) (mkVariable (1, 2) "a") (mkVariable (1, 4) "a")) (mkVariable (1, 7) "a"))),
      ( "(λa.λb.a) 42",
        Right (mkApplication (1, 10) (mkLambda (1, 2) "a" (mkLambda (1, 5) "b" (mkVariable (1, 8) "a"))) (litInt (1, 11) 42))
      ),
      ( "(λa.λb.a) 42 69",
        Right
          ( mkApplication
              (1, 13)
              (mkApplication (1, 10) (mkLambda (1, 2) "a" (mkLambda (1, 5) "b" (mkVariable (1, 8) "a"))) (litInt (1, 11) 42))
              (litInt (1, 14) 69)
          )
      ),
      ( "f 42 69",
        Right (mkApplication (1, 5) (mkApplication (1, 2) (mkVariable (1, 1) "f") (litInt (1, 3) 42)) (litInt (1, 6) 69))
      )
    ]
