module Top where
import Interpreter
import Parser
import Text.Parsec (parse)

toplevel s = case parse exprs "toplevel" s of
                  Left err -> putStrLn (show err)
                  Right expr -> putStrLn (show (eval expr))
