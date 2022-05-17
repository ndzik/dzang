module Dzang.Interpreter where

import Data.List
import Dzang.Language

data Value
  = VBool Bool
  | VInt Integer
  | VFun Expression
  deriving (Eq)

instance Show Value where
  show (VBool b) = show b
  show (VInt i) = show i
  show (VFun expr) = show expr

type Environment = [(Name, Expression)]

evalEval :: String -> IO ()
evalEval input = print $ runEval input

runEval :: String -> Value
runEval input = eval [] (parseDzang input)

runExpr :: Expression -> Value
runExpr = eval []

evalExpr :: Expression -> IO ()
evalExpr = print . eval []

-- Interpreter evaluating simple expressions.
eval :: Environment -> Expression -> Value
eval env (Add lhs rhs) = evalAdd (eval env lhs) (eval env rhs)
eval env (Sub lhs rhs) = evalSub (eval env lhs) (eval env rhs)
eval env (Mul lhs rhs) = evalMul (eval env lhs) (eval env rhs)
eval env (Div lhs rhs) = evalDiv (eval env lhs) (eval env rhs)
eval _ (Literal lit) = evalLiteral lit
eval env app@(Application _ _) = evalApplication env [] app
eval env (Variable vname) = evalVariable env vname
eval env (Lambda vname expr) = evalLambda env vname expr
eval _ (Module _ _) = error "modules not supported (yet)"
eval env (Definition n expr) = evalDefinition env n expr

evalLiteral :: Lit -> Value
evalLiteral (LitInt i) = VInt i
evalLiteral (LitBool b) = VBool b

evalVariable :: Environment -> Name -> Value
evalVariable env name = case find (\(v, _) -> v == name) env of
  Nothing -> error $ "unsaturated variable found: " <> name
  Just (_, expr) -> eval env expr

evalAdd :: Value -> Value -> Value
evalAdd (VInt lhs) (VInt rhs) = VInt $ lhs + rhs
evalAdd _ _ = error "mismatched types on addition"

evalSub :: Value -> Value -> Value
evalSub (VInt lhs) (VInt rhs) = VInt $ lhs - rhs
evalSub _ _ = error "mismatched types on subtraction"

evalMul :: Value -> Value -> Value
evalMul (VInt lhs) (VInt rhs) = VInt $ lhs * rhs
evalMul _ _ = error "mismatched types on multiplication"

evalDiv :: Value -> Value -> Value
evalDiv (VInt lhs) (VInt rhs) = VInt $ lhs `div` rhs
evalDiv _ _ = error "mismatched types on division"

evalLambda :: Environment -> Name -> Expression -> Value
evalLambda env _ = eval env

evalApplication :: Environment -> [Expression] -> Expression -> Value
evalApplication env params (Application inner outer) = evalApplication env (outer : params) inner
evalApplication env (p : ps) (Lambda var body@(Lambda _ _)) = evalApplication ((var, p) : env) ps body
evalApplication env (p : _) (Lambda var body) = evalLambda ((var, p) : env) var body
evalApplication env params (Variable var) = case lookup var env of
  Just lambda@(Lambda _ _) -> evalApplication env params lambda
  _ -> error "applying arguments to non function"
evalApplication _ _ _ = error "error applying arguments to function"

evalDefinition :: Environment -> Name -> Expression -> Value
evalDefinition _ n _ = VFun (Variable n)
