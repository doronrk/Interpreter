--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Test.HUnit

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

contains' :: [Int] -> Int -> Bool
contains' [] _ = False
contains' (x:xs) y = x==y || contains' xs y

testNumb0 = TestCase (assertEqual "Numb 0" 0 (eval (Numb 0) []))
testNumb1 = TestCase (assertEqual "Numb 1" 1 (eval (Numb 1) []))
testPlus = TestCase (assertEqual "Plus (Numb 1) (Numb 2)" 3 (eval (Plus (Numb 1) (Numb 2)) []))

tests = TestList [testNumb0, testNumb1, testPlus]

main = do runTestTT tests
