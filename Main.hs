module Main where

import System.Environment (getArgs)
import Control.Monad (when)

import MiniLangSyntax
import MiniLangParser
import MiniLangEvaluator

-- parse the program, run the commands in it, and evaluate
-- the last expression
parseRun :: String -> (Program, Expr)
parseRun s = do
    case parse start s of 
      [(prog, _)] -> (prog, run prog)
      _ -> error "Parse error"

-- read the file and run it
-- This can be called from the interpreter as readRun True|False "myfile"
readRun :: Bool -> String -> IO Expr
readRun verbose fn = do 
  s <- readFile fn 
  let (p, r) = parseRun s
  when verbose
    ( putStrLn "------ Program ------" >>
      putStrLn (pp 0 p) >>
      putStrLn "------ Result  ------"
    )
  return $ r

main = do 
  -- Minimum one and maximum two command line parameters are required
  -- If the first argument is "-v", more verbose output is provided
  -- Otherwise the first argument must be the filename
  args <- getArgs
  let verbose = args!!0 == "-v"
  let fn = if verbose then args!!1 else args!!0
  res <- readRun verbose fn
  putStrLn $ show res
