module Dzang.Interpreter
  ( InterpreterState(..)
  , Interpreter
  , forever
  , runInterpreter
  , emptyInterpreterState
  , MonadInterpreter(..)
  , InterpreterResult(..)
  , interpret
  , Value(..)
  ) where

import           Control.Monad.State     hiding ( forever )
import           Data.List
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
type Interpreter m a = StateT InterpreterState m a

data InterpreterResult = IR Value PolyType

instance Show InterpreterResult where
  show (IR v pt) = show v <> ": " <> show pt


-- MonadInterpreter allows injecting the concrete monad instance.
class Monad m => MonadInterpreter m where
  log :: InterpreterResult -> m ()
  getInput :: m String

instance MonadInterpreter IO where
  log      = pretty
  getInput = getLine

pretty :: Show a => a -> IO ()
pretty a = putStrLn $ "▷ " <> show a

getInput' :: MonadInterpreter m => Interpreter m String
getInput' = lift getInput

log' :: (MonadInterpreter m) => InterpreterResult -> Interpreter m ()
log' = lift . log

runInterpreter
  :: MonadInterpreter m
  => Interpreter m a
  -> InterpreterState
  -> m (a, InterpreterState)
runInterpreter = runStateT

interpret :: MonadInterpreter m => Interpreter m Value
interpret = getInput' >>= parse >>= eval

parse :: Monad m => String -> Interpreter m Expression
parse = return . parseDzang

typeIt :: Monad m => Expression -> Interpreter m PolyType
typeIt expr = gets typeEnvironment >>= \env ->
  let (pt, env') = runTypeChecker' env expr
  in  modify (\is -> is { typeEnvironment = env' }) >> return pt

forever :: MonadInterpreter m => Interpreter m ()
forever = getInput' >>= parse >>= \expr -> do
  pt <- typeIt expr
  v  <- eval expr
  log' (IR v pt) >> forever

-- Interpreter evaluating simple expressions.
eval :: Monad m => Expression -> Interpreter m Value
eval (    Add lhs rhs          ) = evalAdd <$> eval lhs <*> eval rhs
eval (    Sub lhs rhs          ) = evalSub <$> eval lhs <*> eval rhs
eval (    Mul lhs rhs          ) = evalMul <$> eval lhs <*> eval rhs
eval (    Div lhs rhs          ) = evalDiv <$> eval lhs <*> eval rhs
eval (    Literal lit          ) = evalLiteral lit
eval app@(Application _ _      ) = evalApplication [] app
eval (    Variable vname       ) = evalVariable vname
eval (    Lambda     vname expr) = evalLambda vname expr
eval (    Module     _     _   ) = error "modules not supported (yet)"
eval (    Definition n     expr) = evalDefinition n expr

evalLiteral :: Monad m => Lit -> Interpreter m Value
evalLiteral (LitInt  i) = return $ VInt i
evalLiteral (LitBool b) = return $ VBool b

evalVariable :: Monad m => Name -> Interpreter m Value
evalVariable name = gets valueEnvironment >>= \env ->
  case find (\(v, _) -> v == name) env of
    Nothing        -> error $ "unsaturated variable found: " <> name
    Just (_, expr) -> eval expr

evalAdd :: Value -> Value -> Value
evalAdd (VInt lhs) (VInt rhs) = VInt $ lhs + rhs
evalAdd _          _          = error "mismatched types on addition"

evalSub :: Value -> Value -> Value
evalSub (VInt lhs) (VInt rhs) = VInt $ lhs - rhs
evalSub _          _          = error "mismatched types on subtraction"

evalMul :: Value -> Value -> Value
evalMul (VInt lhs) (VInt rhs) = VInt $ lhs * rhs
evalMul _          _          = error "mismatched types on multiplication"

evalDiv :: Value -> Value -> Value
evalDiv (VInt lhs) (VInt rhs) = VInt $ lhs `div` rhs
evalDiv _          _          = error "mismatched types on division"

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
    _                        -> error "applying arguments to non function"
evalApplication _ _ = error "error applying arguments to function"

evalDefinition :: Monad m => Name -> Expression -> Interpreter m Value
evalDefinition n expr = addToValueEnv (n, expr) >> return (VFun $ Variable n)

addToValueEnv :: Monad m => (Name, Expression) -> Interpreter m ()
addToValueEnv i =
  modify (\s@(InterpreterState env _) -> s { valueEnvironment = i : env })
