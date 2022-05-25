module Dzang.Test.Project where

import Control.Monad.Writer
import Dzang.Project

newtype TestProject = TestProject String deriving (Show)

instance Semigroup TestProject where
  TestProject p1 <> TestProject p2 = TestProject $ unlines [p1, p2]

instance Monoid TestProject where
  mempty = TestProject ""

instance ModuleRef TestProject where
  content (TestProject s) = s

mkMod :: Writer TestProject () -> TestProject
mkMod = execWriter

tt :: String -> Writer TestProject ()
tt = tell . TestProject
