module MiniLangSyntax where

-- This it the abstract syntax of our small language
-- First the expressions in the language

data Expr = IntLit Int 
          | BoolLit Bool
          | BinOp Op Expr Expr
          | Var String
            deriving Eq

-- The different kinds of operations that are supported
data Op = OpPlus | OpMinus | OpMultiplies | OpEqual | OpNotEqual
        deriving Eq

-- Statements of our language
data Stmt = Skip 
          | If Expr Stmt Stmt
          | Assignment String Expr

-- A program is a list of statements, followed by a single expression
data Program = Program [Stmt] Expr

instance PP Program where
  pp _ (Program ss e) =
      concatMap (pp 0) ss ++ show e

-- The primitive functions of our language
primEval OpPlus (IntLit i) (IntLit j) = IntLit $ i + j
primEval OpMinus (IntLit i) (IntLit j) = IntLit $ i - j
primEval OpMultiplies (IntLit i) (IntLit j) = IntLit $ i * j
primEval OpEqual (IntLit i) (IntLit j) = BoolLit $ i == j
primEval OpEqual (BoolLit i) (BoolLit j) = BoolLit $ i == j
primEval OpNotEqual (IntLit i) (IntLit j) = BoolLit $ i /= j
primEval OpNotEqual (BoolLit i) (BoolLit j) = BoolLit $ i /= j
primEval x l r = error $ 
                 "ICE: no definition for primitive operation: " ++ show x ++ " " ++ show l ++ " " ++ show r

-- A helper function to turn a string representation to the opcode, 
-- to be used in the parser
stringToOp :: String -> Op
stringToOp "+" = OpPlus
stringToOp "-" = OpMinus
stringToOp "*" = OpMultiplies
stringToOp "==" = OpEqual
stringToOp "<>" = OpNotEqual


indent :: Int -> String
indent n = replicate n ' '

-- A tiny bit of smarts on how to lay out code, so that it is less tedious to read
class PP a where 
  pp :: Int -> a -> String

instance PP Stmt where
  pp ind (If c t e) = indent ind ++ 
                      "if (" ++ show c ++ ") \n" ++ 
                      pp (ind + 2) t ++
                      indent ind ++ "else\n" ++
                      pp (ind + 2) e
  pp ind (Skip) = indent ind ++ ";\n"
  pp ind (Assignment v e) = indent ind ++ v ++ "=" ++ show e ++ ";\n"

instance Show Op where
         show OpPlus = "+"
         show OpMinus = "-"
         show OpMultiplies = "*"
         show OpEqual = "=="
         show OpNotEqual = "<>"

instance Show Expr where
    show (IntLit i) = show i
    show (BoolLit b) = show b
    show (Var x) = x
    show (BinOp op a b) = addParens $ show a ++ show op ++ show b

addParens s = "(" ++ s ++ ")"
