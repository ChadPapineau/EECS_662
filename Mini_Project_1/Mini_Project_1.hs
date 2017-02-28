{-
********************************************* 
*********************************************
*****	Author: 	Chad Papineau		*****
*****	KU ID: 		2592463				*****
*****	Class: 		EECS 662			*****
*****	Assignment: Mini_Project_1.hs	*****
*****	Date:		February 23, 2017	*****
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
data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mult :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show, Eq)

-- Abstract Syntax Pretty Printers
pprint :: ABE -> String
pprint (Num n) = show n
pprint (Boolean b) = show b
pprint (Plus n m) = "(" ++ pprint n ++ "+" ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ "-" ++ pprint m ++ ")"
pprint (Mult n m) = "(" ++ pprint n ++ "*" ++ pprint m ++ ")"
pprint (Div n m) = "(" ++ pprint n ++ "/" ++ pprint m ++ ")"
pprint (And n m) = "(" ++ pprint n ++ "&&" ++ pprint m ++ ")"
pprint (Leq n m) = "(" ++ pprint n ++ "<=" ++ pprint m ++ ")"
pprint (IsZero m) = "(isZero " ++ pprint m ++ ")"
pprint (If c n m) = "(if " ++ pprint c ++ " then " ++ pprint n ++ " else " ++ pprint m ++ ")"

-- Create parser called 'expr'
-- Parser 'expr' is of type 'Parser ABE'
-- This parser generates AE structures
-- 'buildExpressionParser' constructs the parser for ABE on 
-- operatores defined by 'operators' and terms defined
-- by 'terms'
expr :: Parser ABE
expr = buildExpressionParser operators term

-- Define the operators table that will be used to
-- generate expressions
operators = [ [ inFix "*" Mult AssocLeft,
                inFix "/" Div AssocLeft ],
              [ inFix "+" Plus AssocLeft, 
                inFix "-" Minus AssocLeft ],
              [ inFix "<=" Leq AssocLeft,
                preFix "isZero" IsZero ],
              [ inFix "&&" And AssocLeft ]
            ]

-- Simple parser for numbers
-- 'integer' parser is called and its value stored in 'i'
-- 'Int' is extracted from 'i' and returned in a 'Num' constructor
numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

-- '<|>' operation is an OR operation for parsers
-- 'term' is either a parenthesized expression or an integer
term = parens lexer expr 
       <|> numExpr
       <|> ifExpr
       <|> trueExpr
       <|> falseExpr

parseABE = parseString expr
parseABEFile = parseFile expr

eval :: ABE -> (Either String ABE)
eval (Num t) = (Right (Num t))
eval (Boolean b) = (Right (Boolean b))


eval (Plus t1 t2) = let r1 = (eval t1)
                        r2 = (eval t2)
                    in case r1 of (Left m) -> r1
                                  (Right (Num v1)) -> case r2 of (Left m) -> r2
                                                                 (Right (Num v2)) -> (Right (Num (v1+v2)))
                                                                 (Right _) -> (Left "Type Error in +")
                                  (Right _) -> (Left "Type Error in +")

eval (Minus t1 t2) = let r1 = (eval t1)
                         r2 = (eval t2)
                     in case r1 of (Left m) -> r1
                                   (Right (Num v1)) -> case r2 of (Left m) -> r2
                                                                  (Right (Num v2)) -> (Right (Num (v1-v2)))
                                                                  (Right _) -> (Left "Type Error in -")
                                   (Right _) -> (Left "Type Error in -")

eval (Mult t1 t2) = let r1 = (eval t1)
                        r2 = (eval t2)
                    in case r1 of (Left m) -> r1
                                  (Right (Num v1)) -> case r2 of (Left m) -> r2
                                                                 (Right (Num v2)) -> (Right (Num (v1*v2)))
                                                                 (Right _) -> (Left "Type Error in *")
                                  (Right _) -> (Left "Type Error in *")

eval (Div t1 t2) = let r1 = (eval t1)
                       r2 = (eval t2)
                   in case r1 of (Left m) -> r1
                                 (Right (Num v1)) -> case r2 of (Left m) -> r2
                                                                (Right (Num v2)) -> case v2 of 0 -> (Left "Error - cannot divide by zero")
                                                                                               _ -> (Right (Num (div v1 v2)))
                                                                (Right _) -> (Left "Type Error in /")
                                 (Right _) -> (Left "Type Error in /")

eval (And t1 t2) = let r1 = (eval t1)
                       r2 = (eval t2)
                   in case r1 of (Left m) -> r1
                                 (Right (Boolean v1)) -> case r2 of (Left m) -> r2
                                                                    (Right (Boolean v2)) -> (Right (Boolean (v1&&v2)))
                                                                    (Right _) -> (Left "Type Error in &&")
                                 (Right _) -> (Left "Type Error in &&")

eval (Leq t1 t2) = let r1 = (eval t1)
                       r2 = (eval t2)
                   in case r1 of (Left m) -> r1
                                 (Right (Num v1)) -> case r2 of (Left m) -> r2
                                                                (Right (Num v2)) -> (Right (Boolean (v1<=v2)))
                                                                (Right _) -> (Left "Type Error in <=")
                                 (Right _) -> (Left "Type Error in <=")

eval (IsZero t) = let r = (eval t)
                  in case r of (Left m) -> r
                               (Right (Num v)) -> (Right (Boolean (v==0)))
                               (Right _) -> (Left "Type Error in isZero")

eval (If t1 t2 t3) = let r = (eval t1)
                     in case r of (Left _) -> r
                                  (Right (Boolean v)) -> if v then (eval t2) else (eval t3)
                                  (Right _) -> (Left "Type Error in if")

--interp = eval . parseABE

-- New Abstract Syntax
data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show, Eq)

typeof :: ABE -> (Either String TABE)
typeof (Num x) = (Right TNum)
typeof (Boolean b) = (Right TBool)

typeof (Plus l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if (l' ==TNum && r' ==TNum) then (Right TNum) else (Left "Type Error in +")

typeof (Minus l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if (l' ==TNum && r' ==TNum) then (Right TNum) else (Left "Type Error in -")

typeof (Mult l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if (l' ==TNum && r' ==TNum) then (Right TNum) else (Left "Type Error in *")

typeof (Div l r) = do
  l' <- (typeof l) ;
  r' <- (typeof r) ;
  if (l' ==TNum && r' ==TNum) then case r of
                                     (Num 0) -> (Left "Error - cannot divide by zero")
                                     _ -> (Right (TNum))
                              else (Left "Type Error in /")

typeof (And l r) = do
  l' <- typeof l ;
  r' <- typeof r ;
  if (l' ==TBool && r' ==TBool) then (Right TBool) else (Left "Type Error in &&")

typeof (Leq l r) = do
  l' <- typeof l ;
  r' <- typeof r ;
  case l' of
    TNum -> case r' of
             TNum -> (Right TBool)
             _ -> (Left "Type mismatch in <=")
    TBool -> (Left "Type mismatch in <=")

typeof (IsZero v) = do
  v' <- (typeof v) ;
  if v' == TNum then (Right TBool) else (Left "Type mismatch in IsZero")

typeof (If c t e) = do
  c' <- (typeof c) ;
  t' <- (typeof t) ;
  e' <- (typeof e) ;
  if (c' == TBool && t'==e') then (Right t') else (Left "Type Error in if")

--optimize :: ABE -> Either String ABE
optimize :: ABE -> ABE
optimize (Num x) = (Num x)
optimize (Plus (Num 0) r) = optimize r
optimize (Plus l (Num 0)) = optimize l
optimize (Plus l r) = let l' = (optimize l)
                          r' = (optimize r)
                      in (Plus l' r')

optimize (Minus (Num 0) r) = optimize r
optimize (Minus l r) = let l' = (optimize l)
                           r' = (optimize r)
                       in (Minus l' r')

optimize (Mult (Num 0) r) = (Num 0)
optimize (Mult l (Num 0)) = (Num 0)
optimize (Mult l r) = let l' = (optimize l)
                          r' = (optimize r)
                      in (Mult l' r')

optimize (Div l (Num 1)) = (optimize l)
optimize (Div l r) = let l' = (optimize l)
                         r' = (optimize r)
                     in (Div l' r')

optimize (And (Boolean False) r) = (Boolean False)
optimize (And (Boolean True) r) = (optimize r)

optimize (Leq l r) = let l' = (optimize l)
                         r' = (optimize r)
                     in (Leq l' r')

optimize (IsZero (Num 0)) = (Num 0)

optimize (Boolean b) = (Boolean b)
optimize (If c t e) = (If (optimize c) (optimize t) (optimize e))

interp :: String -> Either String ABE
interp e = let p=(parseABE e) in
                  case (typeof p) of
                    (Right _) -> (eval (optimize p))
                    (Left m) -> (Left m)










