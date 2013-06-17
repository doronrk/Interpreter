module Parser where

import Text.Parsec (parse, parseTest, try)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Char (spaces, letter, alphaNum)
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language(haskellStyle)

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
