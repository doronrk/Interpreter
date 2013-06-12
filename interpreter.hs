--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Test.HUnit

data Expr =
	  Numb Int
	| Plus Expr Expr
	| Minus Expr Expr
	| Times Expr Expr
	| Divide Expr Expr
	| Variable String
	| Let String Expr Expr

eval :: Expr -> Env -> Int
eval e env = case e of 
	Numb x -> x
	Plus e1 e2 -> eval e1 env + eval e2 env
	Minus e1 e2 -> eval e1 env - eval e2 env
	Times e1 e2 -> eval e1 env * eval e2 env
	Divide e1 e2 -> eval e1 env `div` eval e2 env
	Variable s -> eval (getExpr s env) env
	Let s e1 e2 -> eval e2 (addVar s e1 env)
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

testNumb0 = TestCase (assertEqual "Numb 0" 0 (eval (Numb 0) []))
testNumb1 = TestCase (assertEqual "Numb 1" 1 (eval (Numb 1) []))
testPlus = TestCase (assertEqual "Plus (Numb 1) (Numb 2)" 3 (eval (Plus (Numb 1) (Numb 2)) []))
testEnv = TestCase (assertEqual "Variable a 4" 4 (eval (Variable "a") [("a", Numb 4)]))
testLet = TestCase (assertEqual "Let" 2 (eval (Let "x" (Numb 1) (Plus (Variable "x") (Variable "x"))) []))
testLet1 = TestCase (assertEqual "nested Let" 3 (eval (Let "x" (Numb 1) (Let "y" (Numb 2) (Plus (Variable "x") (Variable "y")))) []))

tests = TestList [testNumb0, testNumb1, testPlus, testEnv, testLet, testLet1]

main = do runTestTT tests

