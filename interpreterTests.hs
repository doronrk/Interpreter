module InterpreterTests where
import Interpreter
import Test.HUnit

testNumb0 = TestCase (assertEqual "Numb 0" (Numb 0) (eval (Numb 0)))
testNumb1 = TestCase (assertEqual "Numb 1" (Numb 1) (eval (Numb 1)))
testPlus = TestCase (assertEqual "Plus (Numb 1) (Numb 2)" (Numb 3) (eval (Plus (Numb 1) (Numb 2))))

testBoolean = TestCase (assertEqual "boolean" (Boolean True) (eval (Boolean True)))

testMinus = TestCase (assertEqual "Minus (Numb 4) (Numb 3)" (Numb 1) (eval (Minus (Numb 4) (Numb 3))))
testTimes = TestCase (assertEqual "Times (Numb 3) (Numb 4)" (Numb 12) (eval (Times (Numb 3) (Numb 4))))
testDivide = TestCase (assertEqual "Divide (Numb 12) (Numb 4)" (Numb 3) (eval (Divide (Numb 12) (Numb 4))))

testLet = TestCase (assertEqual "Let" (Numb 2) (eval (Let "x" (Numb 1) (Plus (Variable "x") (Variable "x")))))
testLet1 = TestCase (assertEqual "nested Let" (Numb 3) (eval (Let "x" (Numb 1) (Let "y" (Numb 2) (Plus (Variable "x") (Variable "y"))))))
testCallF1 = TestCase (assertEqual "call expr" (Numb 4) (eval (CallF (Func [] "x" (Plus (Variable "x") (Numb 1))) (Numb 3))))

timesTwo = Func [] "x" (Times (Numb 2) (Variable "x")) -- lambda x = 2*x
myCallF = CallF timesTwo(Numb 4)

testCallF2 = TestCase (assertEqual "jawn" (Numb 8) (eval (myCallF)))

testIf = TestCase (assertEqual "if" (Numb 0) (eval (If (Boolean True) (Numb 0) (Numb 1))))

testClosure = TestCase (assertEqual "closure" (Numb 11) (eval
	(CallF
		(CallF
			(Func [] "x"
				(Func [] "y" (Plus (Variable "x") (Variable "y"))))
			(Numb 5))
		(Numb 6))))

tests = TestList [testNumb0, testNumb1, testPlus, testMinus, testTimes, testDivide, testLet, testLet1, testCallF1, testCallF2, testBoolean, testIf, testClosure]

main = do runTestTT tests
