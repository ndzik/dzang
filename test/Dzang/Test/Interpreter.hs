{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE GeneralizedNewtypeDeriving#-}
module Dzang.Test.Interpreter where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Dzang.Interpreter
import           Dzang.Language                 ( parseDzang )
import           Dzang.Typing.TypeChecker       ( runTypeChecker )
import           Dzang.Typing.Types             ( PolyType )

newtype MockContext w a = MockContext { ctx :: ReaderT String (Writer [w]) a }
  deriving (Applicative, Functor, Monad)

runMockContext :: MockContext PolyType a -> String -> (a, [PolyType])
runMockContext (MockContext mc) = runWriter . runReaderT mc

instance MonadInterpreter (MockContext PolyType) where
  log      = tell'
  getInput = ask'

ask' :: MockContext w String
ask' = MockContext ask

tell' :: InterpreterResult -> MockContext PolyType ()
tell' (IR _ pt) = MockContext . lift . tell $ [pt]

parseType :: String -> PolyType
parseType = runTypeChecker [] . parseDzang

parseTypeEval :: String -> (Value, PolyType)
parseTypeEval s = (v, pt)
 where
  pt = parseType s
  ((v, _), _) =
    runMockContext (runInterpreter interpret emptyInterpreterState) s

runInterpreterMock :: String -> ((Value, InterpreterState), [PolyType])
runInterpreterMock =
  runMockContext (runInterpreter interpret emptyInterpreterState)

type InterpreterMock a = Interpreter (MockContext PolyType) a
