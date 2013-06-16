module Lexer(Token(..), tokenize) where
import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)
import Test.HUnit

-- lexical tokens:

data Token =
	  TInt Int -- digit digit*
	| TBool Bool
	| TLParen -- '('
	| TRParen -- ')'
	| TPlus -- '+'
	| TMinus -- '-'
	| TTimes -- '*'
	| TDivide -- '/'
	| TEquals -- '='
	| TArrow -- '=>'
	| TId String -- letter (letter | digit)*
	| TLet -- 'let'
	| TIn -- 'in'
	| TFn -- 'fn'
	deriving (Show, Eq)


tokenize :: [Char] -> [Token]
tokenize (c : cs)
	| isSpace c = tokenize cs
tokenize ('(' : cs) = TLParen : tokenize cs
tokenize (')' : cs) = TRParen : tokenize cs
tokenize ('+' : cs) = TPlus : tokenize cs
tokenize ('-' : cs) = TMinus : tokenize cs
tokenize ('*' : cs) = TTimes : tokenize cs
tokenize ('/' : cs) = TDivide : tokenize cs
tokenize ('=' : '>' : cs) = TArrow : tokenize cs
tokenize ('=' : cs) = TEquals : tokenize cs
tokenize all@(c : cs)
	| isDigit c =
		let (digits, rest) = span isDigit all 
		in (TInt (read digits)) : tokenize rest
tokenize all@(c: cs)
	| isAlpha c =
		let (id, rest) = span isAlphaNum all
		in case id of
			"let" -> TLet : tokenize rest
			"in" -> TIn : tokenize rest
			"fn" -> TFn : tokenize rest
			"true" -> (TBool True) : tokenize rest
			"false" -> (TBool False) : tokenize rest
			_ -> (TId id) : tokenize rest
tokenize [] = []
--tokenize (c : cs) =

testParens = TestCase (assertEqual "parens" [TLParen, TRParen] (tokenize "()"))
testOps = TestCase (assertEqual "ops" [TMinus, TTimes, TDivide, TPlus]
					(tokenize "-*/+"))
testWSpace = TestCase (assertEqual "wspace" [TMinus, TTimes, TDivide, TPlus]
						(tokenize "-  *\n\n/ +       "))
testArrow = TestCase (assertEqual "arrow" [TInt 123, TArrow, TInt 456]
						(tokenize "123=>456"))
testEmpty = TestCase (assertEqual "empty" [] (tokenize ""))
testId = TestCase (assertEqual "id" [TLParen, TId "a1ad", TRParen] (tokenize "(a1ad)"))
testLet = TestCase (assertEqual "let" [TLet, TId "x", TEquals, TInt 1] (tokenize "let x = 1"))

testLarger = TestCase (assertEqual "larger" [TLet, TId "x", TEquals, TInt 1, TIn, TLParen, TFn, TId "y", TArrow, TId "x", TPlus, TId "y", TRParen] (tokenize "let x=1 in (fn y => x + y)"))

main = do runTestTT (TestList [testParens, testOps, testWSpace, testArrow, testEmpty, testId, testLet, testLarger])