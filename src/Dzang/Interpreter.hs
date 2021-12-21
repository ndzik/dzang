module Dzang.Interpreter where

import           Data.List
import           Dzang.Language

data Value = VBool Bool
           | VInt Integer
           deriving (Eq)

instance Show Value where
  show (VBool b) = show b
  show (VInt  i) = show i

type Environment = [(Name, Expression)]

evalEval :: String -> IO ()
evalEval input = print $ runEval input

runEval :: String -> Value
runEval input = eval [] (parseDzang input)

-- Interpreter evaluating simple expressions.
eval :: Environment -> Expression -> Value
eval env (Add lhs rhs           ) = evalAdd (eval env lhs) (eval env rhs)
eval env (Sub lhs rhs           ) = evalSub (eval env lhs) (eval env rhs)
eval env (Mul lhs rhs           ) = evalMul (eval env lhs) (eval env rhs)
eval env (Div lhs rhs           ) = evalDiv (eval env lhs) (eval env rhs)
eval _   (Literal lit           ) = evalLiteral lit
eval env (Application lambda val) = evalApplication env lambda val
eval env (Variable vname        ) = case find (\(v, _) -> v == vname) env of
  Nothing        -> error "unsaturated variables found"
  Just (_, expr) -> eval env expr
eval env (Lambda     vname expr) = evalLambda env vname expr
eval _   (Module     _     _   ) = error "modules not supported (yet)"
eval _   (Definition _     _   ) = error "definitions not supported (yet)"

evalLiteral :: Lit -> Value
evalLiteral (LitInt  i) = VInt i
evalLiteral (LitBool b) = VBool b

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

evalLambda :: Environment -> Name -> Expression -> Value
evalLambda env _ = eval env

evalApplication :: Environment -> Expression -> Expression -> Value
evalApplication env (Lambda var innerExpr) outerExpr =
  evalLambda ((var, outerExpr) : env) var innerExpr
evalApplication _ expr1 expr2 =
  error
    $  "expected lambda application but got: "
    ++ show expr1
    ++ " applied to: "
    ++ show expr2
