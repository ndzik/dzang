{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE GeneralizedNewtypeDeriving#-}
module Dzang.Test.Interpreter where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Dzang.Interpreter.Error
import           Dzang.Interpreter.Interpreter
import           Dzang.Language                 ( parseDzang )
import           Dzang.Typing.TypeChecker       ( runTypeChecker )
import           Dzang.Typing.Types             ( PolyType )

newtype MockContext w a = MockContext { ctx :: ReaderT String (Writer [w]) a }
  deriving (Applicative, Functor, Monad)

runMockContext :: MockContext String a -> String -> (a, [String])
runMockContext (MockContext mc) = runWriter . runReaderT mc

instance MonadInterpreter (MockContext String) where
  log      = tell'
  getInput = ask'

ask' :: MockContext w String
ask' = MockContext ask

tell' :: Show a => a -> MockContext String ()
tell' = MockContext . lift . tell . (: []) . show

parseType :: String -> PolyType
parseType s = case runTypeChecker [] . parseDzang $ s of
  Right r   -> r
  Left  err -> error . show $ err

parseTypeEval :: String -> (Value, PolyType)
parseTypeEval s = (v, pt)
 where
  pt = parseType s
  v  = case runMockContext (runInterpreter interpret emptyInterpreterState) s of
    (Left  err   , _) -> error $ "interpreter error: " <> show err
    (Right (r, _), _) -> r

runInterpreterMock
  :: String -> Either InterpreterError (Value, InterpreterState)
runInterpreterMock =
  fst . runMockContext (runInterpreter interpret emptyInterpreterState)

evalInterpreterMock :: String -> Value
evalInterpreterMock i = case runInterpreterMock i of
  Left  err -> error $ "interpreter error: " <> show err
  Right r   -> fst r
