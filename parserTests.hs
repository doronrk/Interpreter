module ParserTests where
import Test.HUnit
import Parser
import Interpreter(Expr(..))

testFunc1 = TestCase (assertEqual "fn1" (Func [] "x" (Variable "x")) (unsafe exprs "fn x => x"))
testFunc2 = TestCase (assertEqual "fn2" (Func [] "x" (Variable "x")) (unsafe exprs "(fn x => x)"))
testFunc3 = TestCase (assertEqual "fn3" (Func [] "x" (Variable "x")) (unsafe exprs "fn x => (x)"))
testFunc4 = TestCase (assertEqual "fn4" (Func [] "x" (Variable "x")) (unsafe exprs "(fn x => (x))"))

testFunc5 = TestCase (assertEqual "fn5" (Func [] "f" (Func [] "x" (CallF (Variable "f") (Variable "x")))) (unsafe exprs "fn f => fn x => f x"))

testCallF1 = TestCase (assertEqual "app1" (If (CallF (Variable "f") (Variable "x")) (CallF (Variable "g") (Variable "y")) (CallF (Variable "h") (Variable "z")))
                                   (unsafe exprs "if f x then g y else h z"))
testCallF2 = TestCase (assertEqual "app2" (Let "x" (CallF (Variable "f") (Variable "y")) (CallF (Variable "g") (Variable "x"))) (unsafe exprs "let x = f y in g x"))

testNumb   = TestCase (assertEqual   "num" (Numb 123)            (unsafe exprs "123"))
testBoolean1 = TestCase (assertEqual "bool1" (Boolean True)      (unsafe exprs "true"))
testBoolean2 = TestCase (assertEqual "bool2" (Boolean False)     (unsafe exprs "false"))
testVariable    = TestCase (assertEqual    "id" (Variable "foo") (unsafe exprs "foo"))

testTimes1 = TestCase (assertEqual "mul"                            (Times (Numb 1) (Numb 2))                   (unsafe exprs "1 * 2"))
testTimes2 = TestCase (assertEqual "mul should be left assoc"       (Times (Times (Numb 1) (Numb 2)) (Numb 3))  (unsafe exprs "1 * 2 * 3"))
testTimes3 = TestCase (assertEqual "mul & div should be left assoc" (Divide (Times (Numb 1) (Numb 2)) (Numb 3)) (unsafe exprs "1 * 2 / 3"))

testPlus1 = TestCase (assertEqual "add"                            (Plus (Numb 1) (Numb 2))                  (unsafe exprs "1 + 2"))
testPlus2 = TestCase (assertEqual "add should be left assoc"       (Plus (Plus (Numb 1) (Numb 2)) (Numb 3))  (unsafe exprs "1 + 2 + 3"))
testPlus3 = TestCase (assertEqual "add & sub should be left assoc" (Minus (Plus (Numb 1) (Numb 2)) (Numb 3)) (unsafe exprs "1 + 2 - 3"))
testPlus4 = TestCase (assertEqual "add and mul"                    (Plus (Times (Numb 1) (Numb 2)) (Numb 3)) (unsafe exprs "1 * 2 + 3"))

testMinus = TestCase (assertEqual "sub" (Minus (Numb 1) (Numb 2)) (unsafe exprs "1 - 2"))

testIf = TestCase (assertEqual "if" (If (Boolean True) (Numb 0) (Numb 1)) (unsafe exprs "if true then 0 else 1"))

main = do runTestTT (TestList [testFunc1, testFunc2, testFunc3, testFunc4, testFunc5, testCallF1, testCallF2, testNumb, testBoolean1, testBoolean2, testVariable, testTimes1, testTimes2, testTimes3, testPlus1, testPlus2, testPlus3, testPlus4, testMinus, testIf])
