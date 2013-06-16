module Parser where
import Parsing
import Data.Char (isDigit)
import Test.HUnit

-- grammar:
-- expr   -> ( expr )
-- expr   -> term + expr
-- expr   -> term - expr
-- term   -> factor term'
-- term'  -> * factor term'
-- term'  -> / factor term'
-- term'  ->
-- factor -> int
-- factor -> bool
-- factor -> id

data AST = AST_Bool Bool
         | AST_Num Int
         | AST_Id String
         | AST_Add AST AST
         | AST_Mul AST AST
         | AST_Div AST AST
         deriving (Show, Eq)

expr = do token (char '(')
          e <- expr
          token (char ')')
          return e
        +++ Parsing.identifier

ast_num :: Parser AST
ast_num = do n <- token nat
             return (AST_Num n)

ast_bool :: Parser AST
ast_bool = do b <- false +++ true
              return (AST_Bool b)
           where false = do symbol "false"
                            return False
                 true = do symbol "true"
                           return True

ast_id :: Parser AST
ast_id = do id <- identifier
            return (AST_Id id)

factor :: Parser AST
factor = ast_num +++ ast_bool +++ ast_id

term :: Parser AST
term = factor >>= term'

term' :: AST -> Parser AST
term' lhs = (do op <- symbol "*" +++ symbol "/"
                rhs <- factor
                (term' ((if op == "*" then AST_Mul else AST_Div) lhs rhs)))
            +++ return lhs

testExpr = TestCase (assertEqual "expr" [("foo","")] (parse expr "(foo)"))
testNum = TestCase (assertEqual "num" [(AST_Num 123,"456")] (parse factor "123 456"))
testBool = TestCase (assertEqual "bool" [(AST_Bool True,"456")] (parse factor "true 456"))
testId = TestCase (assertEqual "id" [(AST_Id "foo","456")] (parse factor "foo 456"))
testMul = TestCase (assertEqual "mul" [(AST_Mul (AST_Num 1) (AST_Num 2), "")] (parse term "1 * 2"))
testMulLeftAssoc = TestCase (assertEqual "mul should be left assoc" [(AST_Mul (AST_Mul (AST_Num 1) (AST_Num 2)) (AST_Num 3), "")] (parse term "1 * 2 * 3"))
testMulDivLeftAssoc = TestCase (assertEqual "mul & div should be left assoc" [(AST_Div (AST_Mul (AST_Num 1) (AST_Num 2)) (AST_Num 3), "")] (parse term "1 * 2 / 3"))

tests = TestList [testExpr,testNum,testBool,testId,testMul,testMulLeftAssoc,testMulDivLeftAssoc]

main = do runTestTT tests
