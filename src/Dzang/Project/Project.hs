{-# LANGUAGE LambdaCase #-}

module Dzang.Project.Project where

import Dzang.AST
import Dzang.Language
import Dzarser.Base
import Dzarser.Combinator
import Dzarser.Stateful.Parser
import Text.Printf

-- Project describes a Dzang project which is made up of one or more modules.
data Project r = Project String [r] deriving (Show)

-- Module describes a single Dzang module.
data Module = Module String [Expression] deriving (Show, Eq)

-- unmod unwraps the contained modules within the given project.
unmod :: Project r -> [r]
unmod (Project _ r) = r

instance Functor Project where
  fmap f (Project n mods) = Project n $ map f mods

class ModuleRef r where
  content :: r -> String

parseProject :: ModuleRef r => Project r -> [Either ParseError Module]
parseProject p = map (evalParser parseModule) . unmod $ content <$> p

parseModule :: Parser Module
parseModule =
  name >>= \case
    "module" -> parseModuleKey
    _ -> parserFail "expecting module to start with module header declaration"
  where
    parseModuleKey :: Parser Module
    parseModuleKey =
      Module
        <$> (optional spaces *> name)
        <*> (optional spaces *> parseWhere *> parseTopLevelDefinitions emptyEnv)
    parseWhere :: Parser ()
    parseWhere =
      name >>= \case
        "where" -> return ()
        res ->
          parserFail $
            printf "expecting 'where' to close module definition got: %s" res
