--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

data Expr = 
	  Numb Int
	| Plus Expr Expr
	| Minus Expr Expr
	| Times Expr Expr
	| Divide Expr Expr
	| Variable String

eval :: Expr -> Env -> Int
eval e env = case e of 
	Numb x -> x
	Plus e1 e2 -> eval e1 env + eval e2 env
	Minus e1 e2 -> eval e1 env - eval e2 env
	Times e1 e2 -> eval e1 env * eval e2 env
	Divide e1 e2 -> eval e1 env `div` eval e2 env
	Variable s -> eval (getExpr s env) env
	-- 2a) we need env in each call to eval since there may be variables nested within Expr's
	-- 2b) however, is there a way to avoid writing it in every recursive line since the Env doesn't change as we eval? 
	-- 2c) (at least, I don't think the Env changes since variables are assigned during parsing, correct?)
type Env = [(String, Expr)]

getExpr :: String -> Env -> Expr
getExpr _ [] = error "oh no, unreferenced variable!"
getExpr s ((a,b):xs)
	| a == s = b
	| otherwise = getExpr s xs

addVar :: String -> Expr -> Env -> Env
addVar s e1 env = (s,e1):env


myEnv = addVar "a" (Numb 1) []
myEnv2 = addVar "b" (Numb 2) myEnv
myEnv3 = addVar "three" (Plus (Numb 2) (Numb 1)) myEnv2