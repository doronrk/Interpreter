module Parser where

import Text.Parsec (parse, parseTest, try)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Char (spaces, letter, alphaNum, oneOf)
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language(haskellStyle)

import Interpreter(Expr(..))

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

langDef = Token.LanguageDef { Token.commentStart    = ""
                            , Token.commentEnd      = ""
                            , Token.commentLine     = ""
                            , Token.nestedComments  = False
                            , Token.identStart      = letter
                            , Token.identLetter     = alphaNum <|> oneOf "_'"
                            , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
                            , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
                            , Token.reservedOpNames = []
                            , Token.reservedNames   = ["if", "then", "else", "fn", "=>", "let", "in", "true", "false", "+", "-", "*", "/"]
                            , Token.caseSensitive   = True
                            }

lexer = Token.makeTokenParser langDef

symbol     = Token.symbol lexer
integer    = Token.integer lexer
parens     = Token.parens lexer
identifier = Token.identifier lexer

-- two expressions is an application, or one expression alone
exprs :: Parser Expr
exprs = try (do e1 <- expr
                e2 <- expr
                return (CallF e1 e2))
        <|> expr

expr :: Parser Expr
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
                   return (Func [] id body))
       <|> try (do symbol "let"
                   id <- identifier
                   symbol "="
                   bound <- exprs
                   symbol "in"
                   body <- exprs
                   return (Let id bound body))
       <|> (term >>= expr')

expr' :: Expr -> Parser Expr
expr' lhs = try (do op <- try (symbol "+") <|> symbol "-"
                    rhs <- term
                    (expr' ((if op == "+" then Plus else Minus) lhs rhs)))
            <|> return lhs

astNum :: Parser Expr
astNum = do int <- integer
            return (Numb (fromIntegral int)) -- cast Integer to Int, huh?

astBool :: Parser Expr
astBool = do b <- try false <|> true
             return (Boolean b)
          where false = do symbol "false"
                           return False
                true = do symbol "true"
                          return True

astId :: Parser Expr
astId = do id <- identifier
           return (Variable id)

factor :: Parser Expr
factor = try (parens exprs) <|> try astNum <|> try astBool <|> try astId

term :: Parser Expr
term = factor >>= term'

term' :: Expr -> Parser Expr
term' lhs = try (do op <- try (symbol "*") <|> symbol "/"
                    rhs <- factor
                    (term' ((if op == "*" then Times else Divide) lhs rhs)))
            <|> return lhs

-- used for testing only, bails on erros
unsafe p s = case parse p "(unknown)" s of
                  Left err -> error (show err)
                  Right ast -> ast
