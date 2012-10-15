module MiniLangSyntax where

-- This it the abstract syntax of our small language
-- First the expressions in the language

data Expr = IntLit Int 
          | BoolLit Bool
          | BinOp Op Expr Expr
          | Var String
          | Undefined --added Undefined expression
            deriving Eq

-- The different kinds of operations that are supported (added OpDivides, OpLessThan, OpGreaterThan, OpLessThanEq, OpGreaterThanEq)
data Op = OpPlus | OpMinus | OpMultiplies | OpDivides | OpEqual | OpNotEqual | OpLessThan | OpGreaterThan | OpLessThanEq | OpGreaterThanEq
        deriving Eq

-- Statements of our language (added Declaration, Block, and While)
data Stmt = Skip 
          | If Expr Stmt Stmt
          | Assignment String Expr
          | Declaration String Expr
          | Block [Stmt]
          | While Expr Stmt

-- A program is a list of statements, followed by a single expression
data Program = Program [Stmt] Expr

instance PP Program where
  pp _ (Program ss e) =
      concatMap (pp 0) ss ++ show e

-- The primitive functions of our language (added functions for OpDivides, OpLessThan, OpGreaterThan, OpLessThanEq, OpGreaterThanEq)
primEval OpPlus (IntLit i) (IntLit j) = IntLit $ i + j
primEval OpMinus (IntLit i) (IntLit j) = IntLit $ i - j
primEval OpMultiplies (IntLit i) (IntLit j) = IntLit $ i * j
primEval OpDivides (IntLit i) (IntLit j) = IntLit $ i `div` j
primEval OpEqual (IntLit i) (IntLit j) = BoolLit $ i == j
primEval OpEqual (BoolLit i) (BoolLit j) = BoolLit $ i == j
primEval OpNotEqual (IntLit i) (IntLit j) = BoolLit $ i /= j
primEval OpNotEqual (BoolLit i) (BoolLit j) = BoolLit $ i /= j
primEval OpLessThan (IntLit i) (IntLit j) = BoolLit $ i < j
primEval OpGreaterThan (IntLit i) (IntLit j) = BoolLit $ i > j
primEval OpLessThanEq (IntLit i) (IntLit j) = BoolLit $ i <= j
primEval OpGreaterThanEq (IntLit i) (IntLit j) = BoolLit $ i >= j
primEval x l r = error $ 
                 "ICE: no definition for primitive operation: " ++ show x ++ " " ++ show l ++ " " ++ show r

-- A helper function to turn a string representation to the opcode, 
-- to be used in the parser (added  helper functions for OpDivide, OpLessThan, OpGreaterThan, OpLessThanEq, OpGreaterThanEq)
stringToOp :: String -> Op
stringToOp "+" = OpPlus
stringToOp "-" = OpMinus
stringToOp "*" = OpMultiplies
stringToOp "/" = OpDivides
stringToOp "==" = OpEqual
stringToOp "<>" = OpNotEqual
stringToOp "<" = OpLessThan
stringToOp ">" = OpGreaterThan
stringToOp "<=" = OpLessThanEq
stringToOp ">=" = OpGreaterThanEq


indent :: Int -> String
indent n = replicate n ' '

-- A tiny bit of smarts on how to lay out code, so that it is less tedious to read
class PP a where 
  pp :: Int -> a -> String

instance PP Stmt where --added code for Block, Declaration, While
  pp ind (If c t e) = indent ind ++ 
                      "if " ++ show c ++ " \n" ++ 
                      pp (ind + 2) t ++
                      indent ind ++ "else\n" ++
                      pp (ind + 2) e
  pp ind (Skip) = indent ind ++ ";\n"
  pp ind (Block s) = indent ind ++ "{\n" ++ foldl (++) "" (map (pp (ind + 2)) s) ++ indent ind ++ "}\n"
  pp ind (Declaration v e) = indent ind ++ "var " ++ v ++ end
    where
      end = if (e /= Undefined)
              then "=" ++ show e ++ ";\n"
              else ";\n"
  pp ind (While c b) = indent ind ++
                      "while " ++ show c ++ " \n" ++
                      pp (ind + 2) b
  pp ind (Assignment v e) = indent ind ++ v ++ "=" ++ show e ++ ";\n"

instance Show Op where --added OpDivides, OpLessThan, OpGreaterThan, OpLessThanEq, OpGreaterThanEq)
         show OpPlus = "+"
         show OpMinus = "-"
         show OpMultiplies = "*"
         show OpDivides = "/"
         show OpEqual = "=="
         show OpNotEqual = "<>"
         show OpLessThan = "<"
         show OpGreaterThan = ">"
         show OpLessThanEq = "<="
         show OpGreaterThanEq = ">="

instance Show Expr where
    show (IntLit i) = show i
    show (BoolLit b) = show b
    show (Var x) = x
    show (BinOp op a b) = addParens $ show a ++ show op ++ show b
    show Undefined = "previously declared, undefined, or out of scope."

addParens s = "(" ++ s ++ ")"
