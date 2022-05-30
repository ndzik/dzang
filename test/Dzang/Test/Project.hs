module Dzang.Test.Project where

import Control.Monad.Writer
import Dzang.Project.Project
import System.IO.Temp

data TestProject = TestProject String String deriving (Show)

instance Semigroup TestProject where
  (TestProject n p1) <> (TestProject _ p2) = TestProject n $ unlines [p1, p2]

instance Monoid TestProject where
  mempty = TestProject "" ""

instance ModuleRef TestProject where
  content (TestProject _ s) = s

mkMod :: String -> Writer TestProject () -> TestProject
mkMod _ = execWriter

tt :: String -> Writer TestProject ()
tt = tell . TestProject ""

withTestModules :: [TestProject] -> (FilePath -> IO ()) -> IO ()
withTestModules ps action = withSystemTempDirectory "dzang-example" callback
  where
    callback pd = do
      mapM_ (\(TestProject n c) -> writeTempFile pd (n <> ".dzang") c) ps
      action pd
