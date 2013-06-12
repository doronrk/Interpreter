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
	-- 1a) shouln't the Variable type of Expr just be a string?
	-- 1b) otherwise we can access the Expr with e1 within eval w/o calling getExpr
	-- 1c) the string will be mapped to an Expr in an Env during parsing, no?
	-- 2a) we need env in each call to eval since there may be variables nested within Expr's
	-- 2b) however, is there a way to avoid writing it in every recursive line since the Env doesn't change as we eval? 
	-- 2c) (at least, I don't think the Env changes since variables are assigned during parsing, correct?)
type Env = [(String, Expr)]

getExpr :: String -> Env -> Expr
getExpr [] _ = error "oh no, unreferenced variable"
getExpr s ((a,b):xs)
	| a == s = b
	| otherwise = getExpr s xs

addVar :: String -> Expr -> Env -> Env
addVar s e1 env = (s,e1):env


a = ("a", (Numb 5))
b = ("b", (Numb 6))

myEnv = [a,b]

myEnv2 = addVar "c" (Numb 7) myEnv

-- 3a) eval c myEnv2 causes "Not in scope: c" since there is no Haskell variable c
-- 3b) if eval only took a string as an argumemt, and we called eval "c" myEnv2, this should work?
-- 3c) to allow 3b), we could use 1a) 