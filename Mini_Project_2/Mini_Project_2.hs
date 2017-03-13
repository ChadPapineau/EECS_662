{-
*****************************************************
*****************************************************
*****	Author: 	Chad Papineau		*****
*****	KU ID: 		2592463			*****
*****	Class: 		EECS 662		*****
*****	Assignment:	Mini_Project_2.hs	*****
*****	Date:		March 16, 2017		*****
*****************************************************
*****************************************************
-}

{-# LANGUAGE GADTs #-}

module Proj2Utils where

-- Imports for QuickCheck
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- Imports for PLIH
import ParserUtils

--
-- Simple caculator with variables extended Booleans and both static and
-- dynamic type checking.
--
-- Source files for the Boolean Binding Arithmetic Expressions (BBAE)
-- language from PLIH
--

-- BBAE AST Definition

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  Seq :: BBAE -> BBAE -> BBAE
  Print :: BBAE -> BBAE
  Cons :: BBAE -> BBAE -> BBAE
  First :: BBAE -> BBAE
  Rest :: BBAE -> BBAE
  IsEmpty :: BBAE -> BBAE
  Empty :: BBAE 
  deriving (Show,Eq)

-- Parser

expr :: Parser BBAE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
          , [ inFix "<=" Leq AssocLeft
            , preFix "isZero" IsZero ]
          , [ inFix "&&" And AssocLeft ]
          ]

numExpr :: Parser BBAE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

identExpr :: Parser BBAE
identExpr = do i <- identifier lexer
               return (Id i)

bindExpr :: Parser BBAE
bindExpr = do reserved lexer "bind"
              i <- identifier lexer
              reservedOp lexer "="
              v <- expr
              reserved lexer "in"
              e <- expr
              return (Bind i v e)

trueExpr :: Parser BBAE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser BBAE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser BBAE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)
            
seqExpr :: Parser BBAE
seqExpr = do reserved lexer "seq"
             f <- expr
             s <- expr
             return (Seq f s)

printExpr :: Parser BBAE
printExpr = do reserved lexer "print"
               t <- expr
               return (Print t)

consExpr :: Parser BBAE
consExpr = do reserved lexer "cons"
              f <- expr
              s <- expr
              return (Cons f s)

firstExpr :: Parser BBAE
firstExpr = do reserved lexer "first"
               t <- expr
               return (First t)
             
restExpr :: Parser BBAE
restExpr = do reserved lexer "rest"
              t <- expr
              return (Rest t)

isEmptyExpr :: Parser BBAE
isEmptyExpr = do reserved lexer "isEmpty"
                 t <- expr
                 return (IsEmpty t)

emptyExpr :: Parser BBAE
emptyExpr = do reserved lexer "empty"
               return Empty
             
term = parens lexer expr
       <|> numExpr
       <|> identExpr
       <|> bindExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
       <|> consExpr
       <|> firstExpr
       <|> restExpr              
       <|> isEmptyExpr
       <|> emptyExpr
       <|> printExpr
       <|> seqExpr


-- Defining an evaluation function that uses subst
-- to define interp
evals :: BBAE -> (Either String BBAE) -- QUESTION 
evals (Num x) = (Right (Num x))
evals (Boolean x) = (Right (Boolean x))
evals (Plus t1 t2) = do
  t1' <- (evals t1)
  t2' <- (evals t2)
  case t1' of
    (Num v1) -> case t2' of
                 (Num v2) -> (Right (Num (v1-v2)))
                 (Boolean _) -> (Left "Type Error in -")
    (Boolean _) -> (Left "Type Error in -")

evals (Minus t1 t2) = do
  t1' <- (evals t1)
  t2' <- (evals t2)
  case t1' of
    (Num v1) -> case t2' of
                 (Num v2) -> (Right (Num (v1+v2)))
                 (Boolean _) -> (Left "Type Error in +")
    (Boolean _) -> (Left "Type Error in +")

evals (And t1 t2) = do
  t1' <- (evals t1)
  t2' <- (evals t2)
  case t1' of
    (Boolean v1) -> case t2' of
                 (Boolean v2) -> (Right (Boolean (v1 && v2)))
                 (Num _) -> (Left "Type Error in &&")
    (Num _) -> (Left "Type Error in &&")

evals (Leq t1 t2) = do
  t1' <- (evals t1)
  t2' <- (evals t2)
  case t1' of
    (Num v1) -> case t2' of
                 (Num v2) -> (Right (Boolean (v1 <= v2)))
                 (Boolean _) -> (Left "Type Error in <=")
    (Boolean _) -> (Left "Type Error in <=")

evals (IsZero t) = do
  t' <- (evals t)
  case t' of
    (Num v) -> (Right (Boolean (v == 0)))
    (Boolean _) -> (Left "Type Error in isZero")

subst :: String -> BBAE -> BBAE -> BBAE -- QUESTION
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l)
                             (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l)
                               (subst i v r))
subst i v (Bind i' v' b') = if i==i'
                               then (Bind i' (subst i v v') b')
                               else (Bind i' (subst i v v')
                                             (subst i v b'))
subst i v (Id i') = if i==i'
                       then v
                       else (Id i')
--interps :: String -> (Either String BBAE) -- QUESTION

parseBAE = parseString expr

parseBAEFile = parseFile expr

-- Parser invocation

parseBBAE = parseString expr

parseBBAEFile = parseFile expr
