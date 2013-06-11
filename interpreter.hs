--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

data Expr = 
	  Numb Int
	| Plus Expr Expr
	| Minus Expr Expr
	| Times Expr Expr
	| Divide Expr Expr

eval :: Expr -> Int
eval e = case e of 
	Numb x -> x
	Plus e1 e2 -> eval e1 + eval e2
	Minus e1 e2 -> eval e1 - eval e2
	Times e1 e2 -> eval e1 * eval e2
	Divide e1 e2 -> eval e1 `div` eval e2


type Env = [(String, Expr)]

lookup' :: Env -> String -> Bool
--lookup [] _ = error "oh no, unreferenced variable"
lookup' (x:xs) v = (fst x) == v

contains' :: [Int] -> Int -> Bool
contains' [] _ = False
contains' (x:xs) y = x==y || contains' xs y