module Parser where
import Parsing
import Data.Char (isDigit)
import Test.HUnit

-- grammar:
-- expr   -> term + expr
-- expr   -> term - expr
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
         | Mul AST AST
         | Div AST AST
         deriving (Show, Eq)

expr = do token (char '(')
          e <- expr
          token (char ')')
          return e
        +++ Parsing.identifier

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
factor = ast_num +++ ast_bool +++ ast_id

term :: Parser AST
term = factor >>= term'

term' :: AST -> Parser AST
term' lhs = (do op <- symbol "*" +++ symbol "/"
                rhs <- factor
                (term' ((if op == "*" then Mul else Div) lhs rhs)))
            +++ return lhs

testExpr = TestCase (assertEqual "expr" [("foo","")] (parse expr "(foo)"))
testNum = TestCase (assertEqual "num" [(Num 123,"456")] (parse factor "123 456"))
testBool = TestCase (assertEqual "bool" [(Bool True,"456")] (parse factor "true 456"))
testId = TestCase (assertEqual "id" [(Id "foo","456")] (parse factor "foo 456"))
testMul = TestCase (assertEqual "mul" [(Mul (Num 1) (Num 2), "")] (parse term "1 * 2"))
testMulLeftAssoc = TestCase (assertEqual "mul should be left assoc" [(Mul (Mul (Num 1) (Num 2)) (Num 3), "")] (parse term "1 * 2 * 3"))
testMulDivLeftAssoc = TestCase (assertEqual "mul & div should be left assoc" [(Div (Mul (Num 1) (Num 2)) (Num 3), "")] (parse term "1 * 2 / 3"))

tests = TestList [testExpr,testNum,testBool,testId,testMul,testMulLeftAssoc,testMulDivLeftAssoc]

main = do runTestTT tests
