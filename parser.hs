module Parser where

import Text.Parsec (parse, parseTest, try)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Char (spaces, letter, alphaNum)
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language(haskellStyle)
import Test.HUnit

-- grammar for the lambda calculus

-- exprs  -> expr expr
-- exprs  -> expr

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

lexer = Token.makeTokenParser haskellStyle
symbol     = Token.symbol lexer
integer    = Token.integer lexer
parens     = Token.parens lexer
identifier = Token.identifier lexer

-- two expressions is an application, or one expression alone
exprs :: Parser AST
exprs = try (do e1 <- expr
                e2 <- expr
                return (App e1 e2))
        <|> expr

expr :: Parser AST
expr = try (do symbol "if"
               e1 <- exprs
               symbol "then"
               e2 <- exprs
               symbol "else"
               e3 <- exprs
               return (If e1 e2 e3))
       <|> try (do symbol "fn"
                   id <- identifier
                   symbol "=>"
                   body <- exprs
                   return (Fn id body))
       <|> try (do symbol "let"
                   id <- identifier
                   symbol "="
                   bound <- exprs
                   symbol "in"
                   body <- exprs
                   return (Let id bound body))
       <|> (term >>= expr')

expr' :: AST -> Parser AST
expr' lhs = try (do op <- try (symbol "+") <|> symbol "-"
                    rhs <- term
                    (expr' ((if op == "+" then Add else Sub) lhs rhs)))
            <|> return lhs

astNum :: Parser AST
astNum = do int <- integer
            return (Num (fromIntegral int)) -- cast Integer to Int, huh?

astBool :: Parser AST
astBool = do b <- try false <|> true
             return (Bool b)
          where false = do symbol "false"
                           return False
                true = do symbol "true"
                          return True

astId :: Parser AST
astId = do id <- identifier
           return (Id id)

factor :: Parser AST
factor = try (parens exprs) <|> try astNum <|> try astBool <|> try astId

term :: Parser AST
term = factor >>= term'

term' :: AST -> Parser AST
term' lhs = try (do op <- try (symbol "*") <|> symbol "/"
                    rhs <- factor
                    (term' ((if op == "*" then Mul else Div) lhs rhs)))
            <|> return lhs

-- used for testing only, bails on erros
unsafe p s = case parse p "(unknown)" s of
                  Left err -> error (show err)
                  Right ast -> ast

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
