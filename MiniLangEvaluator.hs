module MiniLangEvaluator where

import MiniLangSyntax

-- Expression evaluator
eval :: Env -> Expr -> Expr
eval _ (IntLit i) = IntLit i
eval _ (BoolLit b) = BoolLit b
eval env (Var n) = lookupVar env n
eval env (BinOp op a b) = primEval op (eval env a) (eval env b)

-- Env is an association list from variable names to expressions
-- Expressions in the environment are assumed to always be values 
type VarName = String
type Env = [(VarName, Expr)]

-- Scope Key
key = ("$", Undefined)

-- Check if a variable exists in an environment
isVarDefined :: Env -> VarName -> Bool
isVarDefined env n = n `elem` map fst env

-- Add a new variable to the environment (modified for scope)
newVar :: Env -> VarName -> Expr -> Env
newVar env n e = 
  if isVarDefined (activeScope [] env) n then
    error $ "Error: Variable " ++ n ++ " already declared as " ++ show e ++ "."
  else
    (n, e):env
    
-- Return to the most recent key
activeScope :: Env -> Env -> Env
activeScope s [] = s
activeScope s (x:xs) = if x /= key
                        then activeScope (s ++ [x]) xs 
                        else s 

-- Assign to a variable.  (modified to use assignVar)
-- If variable exists, replace its value (using assignVar'). 
-- If it doesn't, report the error for an undeclared variable.
assignVar :: Env -> VarName -> Expr -> Env
assignVar env n e =
  if isVarDefined env n then
    assignVar' env [] n e
  else
    error $ "Error: Variable " ++ n ++ " has not been declared."
      
-- Find the first occurence of the variable
-- change the value
-- return the altered environment
assignVar' :: Env -> Env -> VarName -> Expr -> Env
assignVar' ((n', e'):xs) f n e = 
  if n' == n then
    (f ++ [(n, e)] ++ xs)
  else
    assignVar' xs (f ++ [(n', e')]) n e

-- Look up the value of a variable from the environment
-- If the variable is not found, crash
lookupVar :: Env -> VarName -> Expr
lookupVar ((n, v):_) m | n == m = v
lookupVar (_:env) m = lookupVar env m
lookupVar [] m = error $ "Error: Variable " ++ m ++ " is unknown."

-- Add the key to track scope
inBlock :: Env -> Env
inBlock env = key:env

-- Finds the first("$", Undefined)
outBlock :: Env -> Env
outBlock (x:xs) = if x /= key then outBlock xs else xs

-- Execute statement (modified to include while, block, and declaration)
-- Statments can affect the environment, hence return a new
-- environment
exec :: Env -> Stmt -> Env
exec env Skip = env
exec env (If c t e) = 
    exec env ( if eval env c == BoolLit True then t else e )
exec env (While c b) = 
  if (eval env c) == BoolLit True
    then exec (exec env b) (While c b)
    else exec env Skip
exec env (Block t) = outBlock (foldl exec (inBlock env) t)
exec env (Declaration v e) = assignVar (newVar env v e) v (eval env e)
exec env (Assignment v e) = assignVar env v (eval env e)

-- Running the program means executing each of its statements.
-- The frist statement is executed in the empty environment,
-- the next statement in tne environment resulting from the first
-- statement, and so on
-- The expression of the program is evaluated in the environment
-- produced by the last statement
run :: Program -> Expr
run (Program statements e) =
    eval (foldl exec [] statements) e






