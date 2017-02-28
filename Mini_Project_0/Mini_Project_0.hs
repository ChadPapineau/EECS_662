{-
********************************************* 
*********************************************
*****	Author: 	Chad Papineau		*****
*****	KU ID: 		2592463				*****
*****	Class: 		EECS 662			*****
*****	Assignment: Mini_Project_0.hs	*****
*****	Date:		February 9, 2017	*****
*********************************************
*********************************************
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import ParserUtils

-- Abstract Syntax
data AEE where
  Num :: Int -> AEE
  Plus :: AEE -> AEE -> AEE
  Minus :: AEE -> AEE -> AEE
  Mult :: AEE -> AEE -> AEE
  Div :: AEE -> AEE -> AEE
  If0 :: AEE -> AEE -> AEE -> AEE
  deriving (Show, Eq)

-- Abstract Syntax Pretty Printers
-- Pretty printers are the opposite of a parser
-- This will take an AST and print the concrete syntax of the
-- represented program (Used here for testing purposes)
-- If the original parsed result matches teh re-parsing result for
-- all test cases we have strong evidence the parser is correct
pprint :: AEE -> String
pprint (Num n) = show n
pprint (Plus n m) = "(" ++ pprint n ++ "+" ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ "-" ++ pprint m ++ ")"
pprint (Mult n m) = "(" ++ pprint n ++ "*" ++ pprint m ++ ")"
pprint (Div n m) = "(" ++ pprint n ++ "/" ++ pprint m ++ ")"
pprint (If0 c n m) = "(if " ++ pprint c ++ " then " ++ pprint n ++ " else " ++ pprint m ++ ")"

-- Define the operators table that will be used to
-- generate expressions
operators = [ [ inFix "*" Mult AssocLeft,
                inFix "/" Div AssocLeft ],
              [ inFix "+" Plus AssocLeft, 
                inFix "-" Minus AssocLeft ]
            ]

-- Simple parser for numbers
-- 'integer' parser is called and its value stored in 'i'
-- 'Int' is extracted from 'i' and returned in a 'Num' constructor
numExpr :: Parser AEE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

-- Create parser called 'expr'
-- Parser 'expr' is of type 'Parser AE'
-- This parser generates AE structures
-- 'buildExpressionParser' constructs the parser for AE on 
-- operatores defined by 'operators' and terms defined
-- by 'terms'
expr :: Parser AEE
expr = buildExpressionParser operators term

ifExpr :: Parser AEE
ifExpr = do reserved lexer "if0"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If0 c t e)

-- '<|>' operation is an OR operation for parsers
-- 'term' is either a parenthesized expression or an integer
term = parens lexer expr 
       <|> numExpr
       <|> ifExpr

-- Utility functions for calling the parser on strings and files
-- 'parseAE' takes a single argument, parses it, and
-- returns the resulting AST or displays ressulting errors
parseAEE = parseString expr

-- Interpreter
-- Takes an element of AE and produces an element of AE that is a value
eval :: AEE -> AEE
eval (Num x) = (Num x)
eval (Plus t1 t2) = let (Num v1) = (eval t1)
                        (Num v2) = (eval t2)
                    in (Num (v1+v2))
eval (Minus t1 t2) = let (Num v1) = (eval t1)
                         (Num v2) = (eval t2)
                     in (Num (v1-v2))
eval (Mult t1 t2) = let (Num v1) = (eval t1)
                        (Num v2) = (eval t2)
                    in (Num (v1*v2))
eval (Div t1 t2) = let (Num v1) = (eval t1)
                       (Num v2) = (eval t2)
                   in (Num (div v1 v2))
eval (If0 t1 t2 t3) = let (Num v1) = (eval t1)
                          (Num v2) = (eval t2)
                          (Num v3) = (eval t3)
                      in case v1 of 0 -> (Num v2)
                                    _ -> (Num v3)

-- Define a language interpreter 'interp', which puts everything together
-- 'parse' will be called first and the output will be passed to 'eval'
-- If 'parseAE' throws an error, 'interp' will terminate w/o passing
-- a value to 'eval'
--interp :: String -> AEE
interp = eval . parseAEE









