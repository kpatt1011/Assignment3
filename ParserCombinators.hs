module ParserCombinators where

-- This module provides the basic building blocks for a simple parser

import Prelude hiding (fail)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

data Parser a = P (String -> [(a, String)])
-- We return a list of possible parses, where each parse is a pair of the result,
-- and the input that was not yet consumed. 

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

fail :: Parser a
fail = P $ \_ -> []

item :: Parser Char 
item = P $ \inp -> case inp of 
                     (x:xs) -> [(x, xs)]
                     [] -> []

-- Parse with p or q
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P $ \inp -> case parse p inp of 
                        [] -> parse q inp
                        [(v, inp')] -> [(v, inp')]

-- Make parser a monad so that we can benefit from the do-notation
instance Monad Parser where 
    p >>= q = P $ \inp -> case parse p inp of 
                            [] -> []
                            [(v, inp')] -> let q' = q v in parse q' inp'
    return v = P $ \inp -> [(v, inp)]
                



-----------------------------------
-- Some generally useful parsers --
-----------------------------------

-- Parse a character that satisfies a predicate
sat :: (Char -> Bool) -> Parser Char
sat pred = item >>= \c -> 
           if pred c then return c else fail

-- Parse a particular character
char :: Char -> Parser Char
char x = sat (== x)

digit, letter, alphanum :: Parser Char
digit = sat isDigit
letter = sat isAlpha
alphanum = sat isAlphaNum

-- Parse a particular string
string :: String -> Parser String 
string [] = return []
string (x:xs) = char x >>= \c ->
                string xs >>= \cs ->
                return (c:cs)

-- Identifier start with a letter, followed by any alphanumeric characters
identifier :: Parser String
identifier = letter >>= \l ->
               many alphanum >>= \ls ->
               return (l:ls)

-- Parse a natural number
nat :: Parser Int
nat = many1 digit >>= \s -> return (read s)


many1 :: Parser a -> Parser [a]
many1 p = p >>= \v ->
          many p >>= \vs -> 
          return (v:vs)

-- zero or more occurrences of p
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

-- whitespace
space :: Parser ()
space = many (space1 +++ comment) >> return ()

space1 = many1 (sat isSpace) >> return ()
comment = string "--" >> many (sat (/= '\n')) >> return ()

-- strip whitespace after parsing p
token :: Parser a -> Parser a
token p = p >>= \v ->
          space >>
          return v

-- parse a string, and strip whitespace
symbol :: String -> Parser String
symbol xs = token (string xs)

-- parse p something enclosed in parenthes
parens :: Parser a -> Parser a
parens p = char '(' >> p >>= \v -> char ')' >> space >> return v

