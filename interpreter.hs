{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

data Expr = 
	  Numb Int
	| Plus Expr Expr
	| Minus Expr Expr
	| Times Expr Expr

eval :: Expr -> Int
eval e = case e of 
	Numb x -> x
	Plus e1 e2 -> eval e1 + eval e2
	Minus e1 e2 -> eval e1 - eval e2
	Times e1 e2 -> eval e1 * eval e2