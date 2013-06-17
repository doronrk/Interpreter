module ParserTests where
import Test.HUnit
import Parser

testFn1 = TestCase (assertEqual "fn1" (Fn "x" (Id "x")) (unsafe exprs "fn x => x"))
testFn2 = TestCase (assertEqual "fn2" (Fn "x" (Id "x")) (unsafe exprs "(fn x => x)"))
testFn3 = TestCase (assertEqual "fn3" (Fn "x" (Id "x")) (unsafe exprs "fn x => (x)"))
testFn4 = TestCase (assertEqual "fn4" (Fn "x" (Id "x")) (unsafe exprs "(fn x => (x))"))
testFn5 = TestCase (assertEqual "fn5" (Fn "f" (Fn "x" (App (Id "f") (Id "x")))) (unsafe exprs "fn f => fn x => f x"))

testApp1 = TestCase (assertEqual "app1" (If (App (Id "f") (Id "x")) (App (Id "g") (Id "y")) (App (Id "h") (Id "z"))) (unsafe exprs "if f x then g y else h z"))
testApp2 = TestCase (assertEqual "app2" (Let "x" (App (Id "f") (Id "y")) (App (Id "g") (Id "x"))) (unsafe exprs "let x = f y in g x"))

testNum   = TestCase (assertEqual   "num" (Num 123)    (unsafe exprs "123"))
testBool1 = TestCase (assertEqual "bool1" (Bool True)  (unsafe exprs "true"))
testBool2 = TestCase (assertEqual "bool2" (Bool False) (unsafe exprs "false"))
testId    = TestCase (assertEqual    "id" (Id "foo")   (unsafe exprs "foo"))

testMul1 = TestCase (assertEqual "mul"                            (Mul (Num 1) (Num 2))               (unsafe exprs "1 * 2"))
testMul2 = TestCase (assertEqual "mul should be left assoc"       (Mul (Mul (Num 1) (Num 2)) (Num 3)) (unsafe exprs "1 * 2 * 3"))
testMul3 = TestCase (assertEqual "mul & div should be left assoc" (Div (Mul (Num 1) (Num 2)) (Num 3)) (unsafe exprs "1 * 2 / 3"))

testAdd1 = TestCase (assertEqual "add"                            (Add (Num 1) (Num 2))               (unsafe exprs "1 + 2"))
testAdd2 = TestCase (assertEqual "add should be left assoc"       (Add (Add (Num 1) (Num 2)) (Num 3)) (unsafe exprs "1 + 2 + 3"))
testAdd3 = TestCase (assertEqual "add & sub should be left assoc" (Sub (Add (Num 1) (Num 2)) (Num 3)) (unsafe exprs "1 + 2 - 3"))
testAdd4 = TestCase (assertEqual "add and mul"                    (Add (Mul (Num 1) (Num 2)) (Num 3)) (unsafe exprs "1 * 2 + 3"))

testSub = TestCase (assertEqual "sub" (Sub (Num 1) (Num 2)) (unsafe exprs "1 - 2"))

main = do runTestTT (TestList [testFn1, testFn2, testFn3, testFn4, testFn5, testApp1, testApp2, testNum, testBool1, testBool2, testId, testMul1, testMul2, testMul3, testAdd1, testAdd2, testAdd3, testAdd4, testSub])
