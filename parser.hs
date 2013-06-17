module Parser where
import Parsing
import Data.Char (isDigit)
import Test.HUnit

-- grammar:

-- exprs  -> expr exprs'
-- exprs' -> expr exprs'
-- exprs' ->

-- expr   -> if expr then expr else expr
-- expr   -> fn id => expr
-- expr   -> let id = expr in expr
-- expr   -> term expr'
-- expr'  -> + term expr'
-- expr'  -> - term expr'
-- expr'  ->

-- term   -> factor term'
-- term'  -> * factor term'
-- term'  -> / factor term'
-- term'  ->

-- factor -> int
-- factor -> bool
-- factor -> id
-- factor -> ( expr )

data AST = Bool Bool
         | Num Int
         | Id String
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         | If AST AST AST
         | Let String AST AST
         | Fn String AST
         | App AST AST
         deriving (Show, Eq)

exprs :: Parser AST
exprs = do e1 <- expr
           e2 <- expr
           return (App e1 e2)
        +++ expr

expr :: Parser AST
expr = (do symbol "if"
           e1 <- exprs
           symbol "then"
           e2 <- exprs
           symbol "else"
           e3 <- exprs
           return (If e1 e2 e3))
       +++ (do symbol "fn"
               id <- identifier
               symbol "=>"
               body <- exprs
               return (Fn id body))
       +++ (do symbol "let"
               id <- identifier
               symbol "="
               bound <- exprs
               symbol "in"
               body <- exprs
               return (Let id bound body))
       +++ term >>= expr'

expr' :: AST -> Parser AST
expr' lhs = (do op <- symbol "+" +++ symbol "-"
                rhs <- term
                (expr' ((if op == "+" then Add else Sub) lhs rhs)))
            +++ return lhs

ast_num :: Parser AST
ast_num = do n <- token nat
             return (Num n)

ast_bool :: Parser AST
ast_bool = do b <- false +++ true
              return (Bool b)
           where false = do symbol "false"
                            return False
                 true = do symbol "true"
                           return True

ast_id :: Parser AST
ast_id = do id <- identifier
            return (Id id)

factor :: Parser AST
factor = do token (char '(')
            e <- exprs
            token (char ')')
            return e
         +++ ast_num +++ ast_bool +++ ast_id

term :: Parser AST
term = factor >>= term'

term' :: AST -> Parser AST
term' lhs = (do op <- symbol "*" +++ symbol "/"
                rhs <- factor
                (term' ((if op == "*" then Mul else Div) lhs rhs)))
            +++ return lhs

parseStr s = parse exprs s

testFn1 = TestCase (assertEqual "fn1" [(Fn "x" (Id "x"),"")] (parseStr "fn x => x"))
testFn2 = TestCase (assertEqual "fn2" [(Fn "x" (Id "x"),"")] (parseStr "(fn x => x)"))
testFn3 = TestCase (assertEqual "fn3" [(Fn "x" (Id "x"),"")] (parseStr "fn x => (x)"))
testFn4 = TestCase (assertEqual "fn4" [(Fn "x" (Id "x"),"")] (parseStr "(fn x => (x))"))
testFn5 = TestCase (assertEqual "fn5" [(Fn "f" (Fn "x" (App (Id "f") (Id "x"))),"")] (parseStr "fn f => fn x => f x"))

testApp1 = TestCase (assertEqual "app1" [(If (App (Id "f") (Id "x")) (App (Id "g") (Id "y")) (App (Id "h") (Id "z")),"")] (parseStr "if f x then g y else h z"))
testApp2 = TestCase (assertEqual "app2" [(Let "x" (App (Id "f") (Id "y")) (App (Id "g") (Id "x")),"")] (parseStr "let x = f y in g x"))

testNum = TestCase (assertEqual "num" [(Num 123,"")] (parseStr "123"))
testBool = TestCase (assertEqual "bool" [(Bool True,"")] (parseStr "true"))
testId = TestCase (assertEqual "id" [(Id "foo","")] (parseStr "foo"))

testMul = TestCase (assertEqual "mul" [(Mul (Num 1) (Num 2), "")] (parseStr "1 * 2"))
testMulLeftAssoc = TestCase (assertEqual "mul should be left assoc" [(Mul (Mul (Num 1) (Num 2)) (Num 3), "")] (parseStr "1 * 2 * 3"))
testMulDivLeftAssoc = TestCase (assertEqual "mul & div should be left assoc" [(Div (Mul (Num 1) (Num 2)) (Num 3), "")] (parseStr "1 * 2 / 3"))

testAdd = TestCase (assertEqual "add" [(Add (Num 1) (Num 2), "")] (parseStr "1 + 2"))
testSub = TestCase (assertEqual "sub" [(Sub (Num 1) (Num 2), "")] (parseStr "1 - 2"))
testAddLeftAssoc = TestCase (assertEqual "add should be left assoc" [(Add (Add (Num 1) (Num 2)) (Num 3), "")] (parseStr "1 + 2 + 3"))
testAddSubLeftAssoc = TestCase (assertEqual "add & sub should be left assoc" [(Sub (Add (Num 1) (Num 2)) (Num 3), "")] (parseStr "1 + 2 - 3"))
testAddAndMul = TestCase (assertEqual "add and mul" [(Add (Mul (Num 1) (Num 2)) (Num 3), "")] (parseStr "1 * 2 + 3"))

tests = TestList [testFn1,testFn2,testFn3,testFn4,testFn5,testApp1,testApp2,testNum,testBool,testId,testMul,testMulLeftAssoc,testMulDivLeftAssoc,testAdd,testSub,testAddLeftAssoc,testAddSubLeftAssoc,testAddAndMul]

main = do runTestTT tests
