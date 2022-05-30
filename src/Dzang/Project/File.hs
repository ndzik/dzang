module Dzang.Project.File where

import Data.Functor ((<&>))
import Dzang.Project.Project
import System.Directory

newtype FileProject = FileProject String

instance ModuleRef FileProject where
  content (FileProject m) = m

-- readModules reads all dzang modules available at the given filepath.
readModules :: FilePath -> IO [FileProject]
readModules fp = listDirectory fp >>= mapM (readFile . ((fp <> "/") <>)) <&> map FileProject
