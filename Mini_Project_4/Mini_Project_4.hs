{-
*****************************************************
*****************************************************
*****          Author:   Chad Papineau          *****
*****          KU ID:    2592463                *****
*****          Class:    EECS 662               *****
*****          Assignment: Mini_Project_4.hs    *****
*****          Date:   May 05, 2017             *****
*****************************************************
*****************************************************
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Proj4Utils where

import Text.ParserCombinators.Parsec
import Control.Monad
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)

data FBAEValue where
  NumV :: Int -> FBAEValue
  BooleanV :: Bool -> FBAEValue
  ClosureV :: String -> FBAE -> Env -> FBAEValue
  deriving (Show,Eq)

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  Fix :: FBAE -> FBAE
  deriving (Show,Eq)

tokenDef =
  javaStyle { Token.identStart = letter
            , Token.identLetter = alphaNum
            , Token.reservedNames = [ "lambda"
                                    , "bind"
                                    , "in"
                                    , "if"
                                    , "then"
                                    , "else"
                                    , "isZero"
                                    , "true"
                                    , "false"
                                    , "app"
                                    , "Num"
                                    , "Bool"
                                    , "fix" ]
            , Token.reservedOpNames = [ "+","-","*","/","&&","||","<=","=",":","->"]
            }

lexer = Token.makeTokenParser tokenDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

-- Term parser

expr :: Parser FBAE
expr = buildExpressionParser operators term

operators = [ [Infix (reservedOp "*" >> return (Mult )) AssocLeft,
               Infix (reservedOp "/" >> return (Div )) AssocLeft ]
            , [Infix (reservedOp "+" >> return (Plus )) AssocLeft,
               Infix (reservedOp "-" >> return (Minus )) AssocLeft ]
            , [Infix (reservedOp "&&" >> return (And )) AssocLeft,
               Infix (reservedOp "||" >> return (Or )) AssocLeft]
            , [Infix (reservedOp "<=" >> return (Leq )) AssocLeft ]
            , [Prefix (reserved "isZero" >> return (IsZero )) ]
            ]

numExpr :: Parser FBAE
numExpr = do i <- integer
             return (Num (fromInteger i))

trueExpr :: Parser FBAE
trueExpr = do i <- reserved "true"
              return (Boolean True)

falseExpr :: Parser FBAE
falseExpr = do i <- reserved "false"
               return (Boolean False)

ifExpr :: Parser FBAE
ifExpr = do reserved "if"
            c <- expr
            reserved "then"
            t <- expr
            reserved "else"
            e <- expr
            return (If c t e)

identExpr :: Parser FBAE
identExpr = do i <- identifier
               return (Id i)

bindExpr :: Parser FBAE
bindExpr = do reserved "bind"
              i <- identifier
              reservedOp "="
              v <- expr
              reserved "in"
              e <- expr
              return (Bind i v e)

lambdaExpr :: Parser FBAE
lambdaExpr = do reserved "lambda"
                (i,t) <- parens argExpr
                reserved "in"
                b <- expr
                return (Lambda i t b)

argExpr :: Parser (String,TFBAE)
argExpr = do i <- identifier
             reservedOp ":"
             t <- ty
             return (i,t)

appExpr :: Parser FBAE
appExpr = do reserved "app"
             f <- expr
             a <- expr
             return (App f a)

fixExpr :: Parser FBAE
fixExpr = do reserved "fix"
             t <- expr
             return (Fix t)

term = parens expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
       <|> identExpr
       <|> bindExpr
       <|> lambdaExpr
       <|> appExpr
       <|> fixExpr

type Env = [(String,FBAEValue)]
type Cont = [(String,TFBAE)]

eval :: Env -> FBAE -> FBAEValue

eval env (Num x) = (NumV x)

eval env (Boolean b) = (BooleanV b)

eval env (Plus l r) = let (l') = (eval env l)
                          (r') = (eval env r)
                      in case l' of 
                           (NumV ll) -> case r' of
                                          (NumV rr) -> (NumV (ll+rr))
                                          (BooleanV _) -> (error "Type Mismatch in +")
                           (BooleanV _) -> (error "Type Mismatch in +")

eval env (Minus l r) = let (l') = (eval env l)
                           (r') = (eval env r)
                       in case l' of 
                            (NumV ll) -> case r' of
                                           (NumV rr) -> (NumV (ll-rr))
                                           (BooleanV _) -> (error "Type Mismatch in -")
                            (BooleanV _) -> (error "Type Mismatch in -")

eval env (Mult l r) = let (l') = (eval env l)
                          (r') = (eval env r)
                      in case l' of 
                           (NumV ll) -> case r' of
                                          (NumV rr) -> (NumV (ll*rr))
                                          (BooleanV _) -> (error "Type Mismatch in *")
                           (BooleanV _) -> (error "Type Mismatch in *")

eval env (Div l r) = let (l') = (eval env l)
                         (r') = (eval env r)
                     in case l' of 
                          (NumV ll) -> case r' of
                                         (NumV rr) -> (NumV (div ll rr))
                                         (BooleanV _) -> (error "Type Mismatch in /")
                          (BooleanV _) -> (error "Type Mismatch in /")

eval env (And l r) = let (l') = (eval env l)
                         (r') = (eval env r)
                     in case l' of 
                          (BooleanV ll) -> case r' of
                                             (BooleanV rr) -> (BooleanV (ll&&rr))
                                             (NumV _) -> (error "Type Mismatch in &&")
                          (NumV _) -> (error "Type Mismatch in &&")

eval env (Or l r) = let (l') = (eval env l)
                        (r') = (eval env r)
                    in case l' of 
                         (BooleanV ll) -> case r' of
                                            (BooleanV rr) -> (BooleanV (ll||rr))
                                            (NumV _) -> (error "Type Mismatch in ||")
                         (NumV _) -> (error "Type Mismatch in ||")

eval env (Leq l r) = let (l') = (eval env l)
                         (r') = (eval env r)
                     in case l' of 
                          (BooleanV ll) -> case r' of
                                             (BooleanV rr) -> (BooleanV (ll<=rr))
                                             (NumV _) -> (error "Type Mismatch in <=")
                          (NumV _) -> (error "Type Mismatch in <=")

eval env (If c t e) = let (c') = (eval env c)
                      in case c' of
                           (NumV cc) -> (error "Type Mismatch in if")
                           (BooleanV cc) -> if (cc)
                                            then (eval env t)
                                            else (eval env e)

-- QUESTION: What's wrong with this error handling *** replaced '_' with 'Nothing'???
-- QUESTION: Should I replace all the '_' with 'Nothing'???
-- RUNNING: interp "(if (isZero 1) then 1 else x * (app fact x-1))"
-- QUESTION: After reviewing my notes do I need to use helper functions???
eval env (Id id) = case (lookup id env) of
                     Just x -> (x)
                     Nothing -> (error "Variable Not Found!")

eval env (Bind i v b) = let (v') = (eval env v)
                            (b') = (eval ((i,v'):env) b)
                        in (b')

eval env (IsZero v) = let (v') = (eval env v)
                      in case v' of
                           (NumV vv) -> (BooleanV (vv==0))
                           _ -> (error "Type Mismatch in IsZero")

eval env (Lambda i t b) = (ClosureV i b env)

eval env (App f a) = let (f') = (eval env f)
                         (a') = (eval env a)
                     in case f' of
                          (ClosureV i b env') -> (eval ( (i,a'):env' ) b)
                          _ -> (error "Type Mismatch in App")

-- DUMB QUESTION: How come the environmetn 'e' doesn't have to be 'env'???
eval env (Fix f) = let (ClosureV i b e) = (eval env f) in
                    eval e (subst i (Fix (Lambda i TNum b)) b)

subst :: String -> FBAE -> FBAE -> FBAE

subst _ _ (Num x) = (Num x)

subst _ _ (Boolean b) = (Boolean b)

subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))

subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))

subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))

subst i v (Div l r) = (Div (subst i v l) (subst i v r))

subst i v (And l r) = (And (subst i v l) (subst i v r))

subst i v (Or l r) = (Or (subst i v l) (subst i v r))

subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))

subst i v (If c t e) = (If (subst i v c) (subst i v t) (subst i v e))

subst i v (Id id') = if (i==id')
                     then (v)
                     else (Id id')

subst i v (Bind i' v' b') = if (i==i')
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))

subst i v (IsZero x) = (IsZero (subst i v x))

subst i v (Lambda x t b) = (Lambda x t (subst i v b))

subst i v (App f a) = (App f (subst i v a))

subst i v (Fix f) = (Fix (subst i v f)) -- QUESTION: Is this right???

typeof :: Cont -> FBAE -> TFBAE

typeof cont (Num x) = (TNum)

typeof cont (Boolean b) = (TBool)

typeof cont (Plus l r) = let l' = (typeof cont l)
                             r' = (typeof cont r)
                         in if (l'==TNum && r'==TNum)
                            then TNum
                            else error "Type Mismatch in +"

typeof cont (Minus l r) = let l' = (typeof cont l)
                              r' = (typeof cont r)
                          in if (l'==TNum && r'==TNum)
                             then TNum
                             else error "Type Mismatch in -"

typeof cont (Mult l r) = let l' = (typeof cont l)
                             r' = (typeof cont r)
                         in if (l'==TNum && r'==TNum)
                            then TNum
                            else error "Type Mismatch in *"

typeof cont (Div l r) = let l' = (typeof cont l)
                            r' = (typeof cont r)
                        in if (l'==TNum && r'==TNum)
                           then case r of
                                  (Num 0) -> (error "Error - cannot divide by zero")
                                  _ -> TNum
                           else (error "Type Mismatch in /")

typeof cont (And l r) = let l' = (typeof cont l)
                            r' = (typeof cont r)
                        in if (l'==TNum && r'==TNum)
                           then TNum
                           else error "Type Mismatch in &&"

typeof cont (Or l r) = let l' = (typeof cont l)
                           r' = (typeof cont r)
                       in if (l'==TNum && r'==TNum)
                          then TNum
                          else error "Type Mismatch in ||"

typeof cont (Leq l r) = let l' = (typeof cont l)
                            r' = (typeof cont r)
                        in case l' of
                             TNum -> case r' of
                                       TNum -> TBool
                                       _ -> (error "Type Mismatch in <=")
                             TBool -> (error "Type Mismatch in <=")

typeof cont (If c t e) = if (typeof cont c)==TNum && (typeof cont t)==(typeof cont e)
                         then (typeof cont t)
                         else (error "Type Mismatch in if")

--typeof cont (If c t e) = let c' = (typeof cont c)
--                             t' = (typeof cont t)
--                             e' = (typeof cont e)
--                         in if (c'==TBool && t'==e')
--                            then (t')
--                            else (error "Type Mismatch in if")

typeof cont (Id id) = case (lookup id cont) of
                            Just x -> (x)
                            Nothing -> (error "Variable Not Found!")

typeof cont (Bind i v b) = let v' = (typeof cont v)
                           in typeof ((i,v'):cont) b

typeof cont (IsZero v) = let v' = (typeof cont v)
                         in if (v'==TNum)
                            then TBool
                            else (error "Type Mismatch in IsZero")

--typeof cont (Lambda x D b) = let R = typeof ((x,D):cont) b
--                             in D :->: R

typeof cont (App x y) = let tyY = (typeof cont y)
                        in case typeof cont x of
                             tyXd :->: tyXr ->
                               if tyXd==tyY
                               then tyXr
                               else (error "Type Mismatch in app")
                             _ -> (error "First argument not lambda in app")

-- QUESTION: What to do here???
--typeof cont (Fix f) =

interp :: String -> FBAEValue
interp x = eval [] (parseFBAE x)

-- Type parser

ty = buildExpressionParser tyoperators tyTerm

tyoperators = [ [Infix (reservedOp "->" >> return (:->: )) AssocLeft ] ]

tyTerm :: Parser TFBAE
tyTerm = parens ty <|> tyNat <|> tyBool

tyNat :: Parser TFBAE
tyNat = do reserved "Nat"
           return TNum

tyBool :: Parser TFBAE
tyBool = do reserved "Bool"
            return TBool

-- Parser invocation

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

parseFBAE = parseString expr

parseFile p file =
  do program <- readFile file
     case parse p "" program of
       Left e -> print e >> fail "parse error"
       Right r -> return r

parseFBAEFile = parseFile expr

