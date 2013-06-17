--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Interpreter(Expr(..),eval) where

data Expr =
	  Numb Int
	| Boolean Bool
	| Plus Expr Expr
	| Minus Expr Expr
	| Times Expr Expr
	| Divide Expr Expr
	| Variable String
	| Let String Expr Expr
	| Func Env String Expr
	| CallF Expr Expr
	| If Expr Expr Expr

	deriving (Eq)

instance Show Expr where
  show (Numb n) = show n
  show (Boolean b) = show b
  show (Plus e1 e2) = "#+ " ++ show e1 ++ " " ++ show e2
  show (Minus e1 e2) = "#- " ++ show e1 ++ " " ++ show e2
  show (Times e1 e2) = "#* " ++ show e1 ++ " " ++ show e2
  show (Divide e1 e2) = "#/ " ++ show e1 ++ " " ++ show e2
  show (Variable v) = v
  show (Let v e1 e2) = "#<let " ++ v ++ "=" ++ show e1 ++ " in " ++ show e2 ++ ">"
  show (Func env v e) = "#<fn " ++ v ++ " => " ++ show e ++ ">"
  show (CallF e1 e2) = "#<app " ++ show e1 ++ " " ++ show e2 ++ ">"
  show (If e1 e2 e3) = "#<if " ++ show e1 ++ "," ++ show e2 ++ "," ++ show e3 ++ ">"

eval :: Expr -> Expr
eval e = eval' e []

eval' :: Expr -> Env -> Expr
eval' e env =
	let
		handleArith op (Numb arg1) (Numb arg2) = Numb (op arg1 arg2)
		handleArith _ _ _ = error "tried to apply arith op to non-number"
	in case e of 
		Numb x -> Numb x
		Boolean x -> Boolean x
		Plus e1 e2 -> handleArith (+) (eval' e1 env) (eval' e2 env)
		Minus e1 e2 -> handleArith (-) (eval' e1 env) (eval' e2 env)
		Times e1 e2 -> handleArith (*) (eval' e1 env) (eval' e2 env)
		Divide e1 e2 -> handleArith (div) (eval' e1 env) (eval' e2 env)
		Variable s -> eval' (getExpr s env) env
		Let s e1 e2 -> eval' e2 (addVar s e1 env)
		Func _ var body -> Func env var body
		CallF f arg -> case eval' f env of
						Func cenv s body -> eval' body (addVar s arg cenv)
						_ -> error "tried to call non-function"
		If boolean true false -> case eval' boolean env of
						Boolean True -> eval' true env
						Boolean False -> eval' false env
						_ -> error "non-boolean in if"

type Env = [(String, Expr)]

getExpr :: String -> Env -> Expr
getExpr s [] = error ("oh no, unreferenced variable: " ++ s)
getExpr s ((a,b):xs)
	| a == s = b
	| otherwise = getExpr s xs

addVar :: String -> Expr -> Env -> Env
addVar s e1 env = (s,e1):env
