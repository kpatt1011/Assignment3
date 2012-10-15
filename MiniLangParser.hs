module MiniLangParser (parse, start) where 

import Prelude hiding (fail)
import MiniLangSyntax
import ParserCombinators

-- Parsers for the expression language
start :: Parser Program
start = space >> program

comment = string "--" >> many (sat (/= '\n')) >> space >> return ()

-- program is many statements followed by an expression
program = many stmt >>= \s -> 
          expr >>= \e ->
          return $ Program s e

----------------
-- Statements --
----------------

-- It is important that assignment is tried last
-- We want that the alternatives tried first fail at the first token, without consuming input
stmt = skip +++ ifstmt +++ while +++ block +++ declaration +++ declWithAssign +++ assignment

skip = symbol ";" >> return Skip

ifstmt = symbol "if" >> --added code for block, while, declaration, declWithAssign
         parens expr >>= \c ->
         stmt >>= \t ->
         symbol "else" >>
         stmt >>= \e ->
         return $ If c t e
         
while = symbol "while" >>
        parens expr >>= \c ->
        stmt >>= \b ->
        return $ While c b
         
block = symbol "{" >>
        many stmt >>= \c ->
        symbol "}" >>
        return (Block c)
        
declWithAssign = symbol "var" >>
              token identifier >>= \v ->
              symbol "=" >>
              expr >>= \e ->
              symbol ";" >> 
              return (Declaration v e)
              
declaration = symbol "var" >>
              token identifier >>= \v ->
              symbol ";" >> 
              return (Declaration v Undefined) 
         
-- make sure assignment is tried last
assignment = token identifier >>= \v ->
             symbol "=" >>
             expr >>= \e ->
             symbol ";" >> 
             return (Assignment v e) 

-----------------
-- Expressions -- (added comparison, comparsionX (based on current naming scheme)) 
-----------------

expr = composite +++ atomic

atomic = literal +++ varRef +++ parens expr

literal = intLiteral +++ boolLiteral

intLiteral = token nat >>= \i -> return $ IntLit i

boolLiteral = strue +++ sfalse

strue = symbol "true" >> return (BoolLit True)
sfalse = symbol "false" >> return (BoolLit False)

varRef = token identifier >>= \n -> return (Var n)

composite = comparison >>= \left ->
            compositeX left

compositeX left
    = ((symbol "==" +++ symbol "<>") >>= \op -> 
           comparison >>= \right -> 
           compositeX $ BinOp (stringToOp op) left right)
      +++
      return left
      
comparison = summation >>= \left ->
              comparisonX left

comparisonX left
    = ((symbol "<=" +++ symbol ">=" +++ symbol "<" +++ symbol ">") >>= \op -> --all comparsion operators GEQ, LEQ, GT, LT, using compositeX as a template
           summation >>= \right -> 
           comparisonX $ BinOp (stringToOp op) left right)
      +++
      return left

summation = term >>= \left -> 
            summationX left

summationX left = 
    ((symbol "+" +++ symbol "-") >>= \op -> 
         term >>= \right ->
         summationX $ BinOp (stringToOp op) left right)
    +++ 
    return left

term = atomic >>= \left -> 
       termX left

termX left = 
    ((symbol "*" +++ symbol "/") >>= \op ->
         atomic >>= \right ->
         termX $ BinOp (stringToOp op) left right)
    +++
    return left
