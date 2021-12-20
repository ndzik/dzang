module Dzang.Test.InterpreterSpec where

import           Data.Foldable
import           Dzang.Interpreter
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                )
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "Dzang Interpreter" $ do
    it "evaluates simple math expressions"    testSimpleMathEval
    it "evaluates applied lambda expressions" testAppliedLambda

testSimpleMathEval :: Expectation
testSimpleMathEval = do
  traverse_
    (\(input, expec) -> runEval input `shouldBe` expec)
    [("1+1", VInt 2), ("2-1", VInt 1), ("2*2", VInt 4), ("4/2", VInt 2)]

testAppliedLambda :: Expectation
testAppliedLambda = do
  traverse_
    (\(input, expec) -> runEval input `shouldBe` expec)
    [ ("λx.42"          , VInt 42)
    , ("λx.x+42 3"      , VInt 45)
    , ("λx.λy.x+y 69 42", VInt 111)
    , ("λx.λy.x*y λx.42 0 2", VInt 84)
    ]
