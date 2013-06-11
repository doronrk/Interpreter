--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

data Expr = 
	  Numb Int
	| Plus Expr Expr
	| Minus Expr Expr
	| Times Expr Expr
	| Divide Expr Expr
	| Variable (String, Expr)

eval :: Expr -> Env -> Int
eval e env = case e of 
	Numb x -> x
	Plus e1 e2 -> eval e1 env + eval e2 env
	Minus e1 e2 -> eval e1 env - eval e2 env
	Times e1 e2 -> eval e1 env * eval e2 env
	Divide e1 e2 -> eval e1 env `div` eval e2 env
	Variable (s, e1) -> eval (getExpr s env) env


a = ("a", (Numb 5))
b = ("b", (Numb 6))
myEnv = [a,b]

type Env = [(String, Expr)]

getExpr :: String -> Env -> Expr
getExpr [] _ = error "oh no, unreferenced variable"
getExpr v ((a,b):xs)
	| a == v = b
	| otherwise = getExpr v xs