{-#LANGUAGE LambdaCase#-}
{-#LANGUAGE ScopedTypeVariables#-}
module Dzang.Interpreter.Interpreter
  ( InterpreterState(..)
  , Interpreter
  , forever
  , forever'
  , debugLog
  , runInterpreter
  , emptyInterpreterState
  , MonadInterpreter(..)
  , InterpreterResult(..)
  , interpret
  , Value(..)
  ) where

import           Control.Monad.Except    hiding ( forever )
import           Control.Monad.State     hiding ( forever )
import           Data.List
import           Dzang.Interpreter.Error
import           Dzang.Language
import           Dzang.Typing.TypeChecker       ( runTypeChecker' )
import           Dzang.Typing.Types
import           Prelude                 hiding ( log )

data Value
  = VBool Bool
  | VInt Integer
  | VFun Expression
  deriving (Eq)

instance Show Value where
  show (VBool b   ) = show b
  show (VInt  i   ) = show i
  show (VFun  expr) = show expr

data InterpreterState = InterpreterState
  { valueEnvironment :: [(Name, Expression)]
  , typeEnvironment  :: [(Name, PolyType)]
  }

emptyInterpreterState :: InterpreterState
emptyInterpreterState = InterpreterState [] []

-- Interpreter is a monad transformer containing its own environment and
-- running in the context of `m`, which should be a `MonadInterpreter`.
type Interpreter m a = StateT InterpreterState (ExceptT InterpreterError m) a

data InterpreterResult a = IR Value PolyType | IRE a

instance Show a => Show (InterpreterResult a) where
  show (IR v pt) = show v <> ": " <> show pt
  show (IRE s  ) = show s


-- MonadInterpreter allows injecting the concrete monad instance.
class Monad m => MonadInterpreter m where
  log :: Show a => a -> m ()
  getInput :: m String

instance MonadInterpreter IO where
  log      = pretty
  getInput = getLine

pretty :: Show a => a -> IO ()
pretty a = putStrLn $ "▷ " <> show a

getInput' :: MonadInterpreter m => Interpreter m String
getInput' = lift . lift $ getInput

log' :: (Show a, MonadInterpreter m) => a -> Interpreter m ()
log' = lift . lift . log

runInterpreter
  :: (MonadInterpreter m)
  => Interpreter m a
  -> InterpreterState
  -> m (Either InterpreterError (a, InterpreterState))
runInterpreter ip s = runExceptT (runStateT ip s) >>= \case
  Left ie -> log (IRE ie) >> return (Left ie)
  r       -> return r

interpret :: MonadInterpreter m => Interpreter m Value
interpret = getInput' >>= parse >>= eval

parse :: Monad m => String -> Interpreter m Expression
parse = return . parseDzang

typeIt :: Monad m => Expression -> Interpreter m PolyType
typeIt expr = gets typeEnvironment >>= \env -> case runTypeChecker' env expr of
  Right (pt, env') ->
    modify (\is -> is { typeEnvironment = env' }) >> return pt
  Left err -> throwError . TE $ err

forever :: MonadInterpreter m => Interpreter m ()
forever = forever' (return ())

forever' :: MonadInterpreter m => Interpreter m () -> Interpreter m ()
forever' eff = getInput' >>= parse >>= \expr ->
  do
      pt <- typeIt expr
      v  <- eval expr
      log' (IR v pt :: InterpreterResult ())
    `catchError` (log' . IRE)
    >>           eff
    >>           forever' eff

debugLog :: MonadInterpreter m => Interpreter m ()
debugLog =
    gets typeEnvironment
    >>= log'
    >>  gets valueEnvironment
    >>= log'

-- Interpreter evaluating simple expressions.
eval :: Monad m => Expression -> Interpreter m Value
eval (    Add lhs rhs          ) = evalWithPrimitive evalAdd lhs rhs
eval (    Sub lhs rhs          ) = evalWithPrimitive evalSub lhs rhs
eval (    Mul lhs rhs          ) = evalWithPrimitive evalMul lhs rhs
eval (    Div lhs rhs          ) = evalWithPrimitive evalDiv lhs rhs
eval (    Literal lit          ) = evalLiteral lit
eval app@(Application _ _      ) = evalApplication [] app
eval (    Variable vname       ) = evalVariable vname
eval (    Lambda     vname expr) = evalLambda vname expr
eval (    Module     _     _   ) = throwError $ EE "modules not supported (yet)"
eval (    Definition n     expr) = evalDefinition n expr

evalWithPrimitive
  :: Monad m
  => (Value -> Value -> Either InterpreterError Value)
  -> Expression
  -> Expression
  -> Interpreter m Value
evalWithPrimitive op lhs rhs = op <$> eval lhs <*> eval rhs >>= \case
  Right v   -> return v
  Left  err -> throwError err

evalLiteral :: Monad m => Lit -> Interpreter m Value
evalLiteral (LitInt  i) = return $ VInt i
evalLiteral (LitBool b) = return $ VBool b

evalVariable :: Monad m => Name -> Interpreter m Value
evalVariable name = gets valueEnvironment >>= \env ->
  case find (\(v, _) -> v == name) env of
    Nothing        -> throwError (EE $ "unsaturated variable found: " <> name)
    Just (_, expr) -> eval expr

evalAdd :: Value -> Value -> Either InterpreterError Value
evalAdd (VInt lhs) (VInt rhs) = Right . VInt $ lhs + rhs
evalAdd _          _          = Left . EE $ "mismatched types on addition"

evalSub :: Value -> Value -> Either InterpreterError Value
evalSub (VInt lhs) (VInt rhs) = Right . VInt $ lhs - rhs
evalSub _          _          = Left . EE $ "mismatched types on subtraction"

evalMul :: Value -> Value -> Either InterpreterError Value
evalMul (VInt lhs) (VInt rhs) = Right . VInt $ lhs * rhs
evalMul _          _          = Left . EE $ "mismatched types on multiplication"

evalDiv :: Value -> Value -> Either InterpreterError Value
evalDiv (VInt lhs) (VInt rhs) = Right . VInt $ lhs `div` rhs
evalDiv _          _          = Left . EE $ "mismatched types on division"

evalLambda :: Monad m => Name -> Expression -> Interpreter m Value
evalLambda _ = eval

evalApplication :: Monad m => [Expression] -> Expression -> Interpreter m Value
evalApplication params (Application inner outer) =
  evalApplication (outer : params) inner
evalApplication (p : ps) (Lambda var body@(Lambda _ _)) =
  addToValueEnv (var, p) >> evalApplication ps body
evalApplication (p : _) (Lambda var body) =
  addToValueEnv (var, p) >> evalLambda var body
evalApplication params (Variable var) = gets valueEnvironment >>= \env ->
  case lookup var env of
    Just lambda@(Lambda _ _) -> evalApplication params lambda
    _ -> throwError $ EE "applying arguments to non function"
evalApplication _ _ = throwError $ EE "error applying arguments to function"

evalDefinition :: Monad m => Name -> Expression -> Interpreter m Value
evalDefinition n expr = addToValueEnv (n, expr) >> return (VFun $ Variable n)

addToValueEnv :: Monad m => (Name, Expression) -> Interpreter m ()
addToValueEnv i =
  modify (\s@(InterpreterState env _) -> s { valueEnvironment = i : env })
