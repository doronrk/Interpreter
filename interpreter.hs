--{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Test.HUnit

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

	deriving (Show, Eq)

eval :: Expr -> Env -> Expr
eval e env =
	let
		handleArith op (Numb arg1) (Numb arg2) = Numb (op arg1 arg2)
		handleArith _ _ _ = error "tried to apply arith op to non-number"
	in case e of 
		Numb x -> Numb x
		Boolean x -> Boolean x
		Plus e1 e2 -> handleArith (+) (eval e1 env) (eval e2 env)
		Minus e1 e2 -> handleArith (-) (eval e1 env) (eval e2 env)
		Times e1 e2 -> handleArith (*) (eval e1 env) (eval e2 env)
		Divide e1 e2 -> handleArith (div) (eval e1 env) (eval e2 env)
		Variable s -> eval (getExpr s env) env
		Let s e1 e2 -> eval e2 (addVar s e1 env)
		Func _ var body -> Func env var body
		CallF f arg -> case eval f env of
						Func cenv s body -> eval body (addVar s arg cenv)
						_ -> error "tried to call non-function"
		If boolean true false -> case eval boolean env of
						Boolean True -> eval true env
						Boolean False -> eval false env
						_ -> error "non-boolean in if"
	-- 2a) we need env in each call to eval since there may be variables nested within Expr's
	-- 2b) however, is there a way to avoid writing it in every recursive line since the Env doesn't change as we eval? 
	-- 2c) (at least, I don't think the Env changes since variables are assigned during parsing, correct?)


type Env = [(String, Expr)]

getExpr :: String -> Env -> Expr
getExpr s [] = error ("oh no, unreferenced variable: " ++ s)
getExpr s ((a,b):xs)
	| a == s = b
	| otherwise = getExpr s xs

addVar :: String -> Expr -> Env -> Env
addVar s e1 env = (s,e1):env

myEnv = addVar "a" (Numb 1) []
myEnv2 = addVar "b" (Numb 2) myEnv
myEnv3 = addVar "three" (Plus (Numb 2) (Numb 1)) myEnv2

testNumb0 = TestCase (assertEqual "Numb 0" (Numb 0) (eval (Numb 0) []))
testNumb1 = TestCase (assertEqual "Numb 1" (Numb 1) (eval (Numb 1) []))
testPlus = TestCase (assertEqual "Plus (Numb 1) (Numb 2)" (Numb 3) (eval (Plus (Numb 1) (Numb 2)) []))

testBoolean = TestCase (assertEqual "boolean" (Boolean True) (eval (Boolean True) []))

testMinus = TestCase (assertEqual "Minus (Numb 4) (Numb 3)" (Numb 1) (eval (Minus (Numb 4) (Numb 3)) []))
testTimes = TestCase (assertEqual "Times (Numb 3) (Numb 4)" (Numb 12) (eval (Times (Numb 3) (Numb 4)) []))
testDivide = TestCase (assertEqual "Divide (Numb 12) (Numb 4)" (Numb 3) (eval (Divide (Numb 12) (Numb 4)) []))

testEnv = TestCase (assertEqual "Variable a 4" (Numb 4) (eval (Variable "a") [("a", Numb 4)]))
testLet = TestCase (assertEqual "Let" (Numb 2) (eval (Let "x" (Numb 1) (Plus (Variable "x") (Variable "x"))) []))
testLet1 = TestCase (assertEqual "nested Let" (Numb 3) (eval (Let "x" (Numb 1) (Let "y" (Numb 2) (Plus (Variable "x") (Variable "y")))) []))
testCallF1 = TestCase (assertEqual "call expr" (Numb 4) (eval (CallF (Func [] "x" (Plus (Variable "x") (Numb 1) )) (Numb 3)) []))

timesTwo = Func [] "x" (Times (Numb 2) (Variable "x")) -- lambda x = 2*x
myCallF = CallF timesTwo(Numb 4)

testCallF2 = TestCase (assertEqual "jawn" (Numb 8) (eval (myCallF) []))

testIf = TestCase (assertEqual "if" (Numb 0) (eval (If (Boolean True) (Numb 0) (Numb 1)) []))

testClosure = TestCase (assertEqual "closure" (Numb 11) (eval
	(CallF
		(CallF
			(Func [] "x"
				(Func [] "y" (Plus (Variable "x") (Variable "y"))))
			(Numb 5))
		(Numb 6)) []))

tests = TestList [testNumb0, testNumb1, testPlus, testMinus, testTimes, testDivide, testEnv, testLet, testLet1, testCallF1, testCallF2, testBoolean, testIf, testClosure]

main = do runTestTT tests
