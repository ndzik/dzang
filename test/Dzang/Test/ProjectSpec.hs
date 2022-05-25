module Dzang.Test.ProjectSpec where

import Dzang.AST
import Dzang.Project
import Dzang.Test.Project
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = describe "Dzang Project" $ do
  it "parses valid projects" testValidProject
  it "handles invalid projects with errors" testInvalidProjects

testValidProject :: Expectation
testValidProject = do
  parseProject
    ( Project
        "TestProject"
        [ mkMod $ do
            tt "module TestModule where"
            tt ""
            tt "g = λf.λx.λy.(f x y)"
            tt ""
            tt "add = λx.λy.(x+y)"
            tt ""
            tt "h = g add"
            tt "",
          mkMod $ do
            tt "module TestModule where"
            tt "g = λf.λx.λy.(f x y)"
            tt "add = λx.λy.(x+y)"
            tt "h = g add"
        ]
    )
    `shouldBe` [ Right $
                   Module
                     "TestModule"
                     [ mkDefinition (3, 1) "g" $
                         mkLambda (3, 5) "f" $
                           mkLambda (3, 8) "x" $
                             mkLambda
                               (3, 11)
                               "y"
                               (mkApplication (3, 18) (mkApplication (3, 16) (mkVariable (3, 15) "f") (mkVariable (3, 17) "x")) (mkVariable (3, 19) "y")),
                       mkDefinition (5, 1) "add" $
                         mkLambda (5, 7) "x" $
                           mkLambda
                             (5, 10)
                             "y"
                             (mkAdd (5, 15) (mkVariable (5, 14) "x") (mkVariable (5, 16) "y")),
                       mkDefinition
                         (7, 1)
                         "h"
                         (mkApplication (7, 6) (mkVariable (7, 5) "g") (mkVariable (7, 7) "add"))
                     ],
                 Right $
                   Module
                     "TestModule"
                     [ mkDefinition (2, 1) "g" $
                         mkLambda (2, 5) "f" $
                           mkLambda (2, 8) "x" $
                             mkLambda
                               (2, 11)
                               "y"
                               (mkApplication (2, 18) (mkApplication (2, 16) (mkVariable (2, 15) "f") (mkVariable (2, 17) "x")) (mkVariable (2, 19) "y")),
                       mkDefinition (3, 1) "add" $
                         mkLambda (3, 7) "x" $
                           mkLambda
                             (3, 10)
                             "y"
                             (mkAdd (3, 15) (mkVariable (3, 14) "x") (mkVariable (3, 16) "y")),
                       mkDefinition
                         (4, 1)
                         "h"
                         (mkApplication (4, 6) (mkVariable (4, 5) "g") (mkVariable (4, 7) "add"))
                     ]
               ]

testInvalidProjects :: Expectation
testInvalidProjects = do
  parseProject
    ( Project
        "TestProject"
        [ mkMod $ do
            tt "module TestModule where"
            tt ""
            tt "g = λf.λx.λy.(f x y)"
            tt ""
            tt "add = λx.λ.(x+y)"
            tt ""
            tt "h = g add"
            tt ""
        ]
    )
    `shouldBe` [ Left "error at [Col 5 | Line 11]: expected name but got: '.' at: '.(x+y)'"
               ]
