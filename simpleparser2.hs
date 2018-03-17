module Main where 
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces) -- defined our own spaces 


main :: IO ()
main = do args <- getArgs 
		putStrLn (readExpr (args !! 0))

symbol :: Parser Char   -- parser that recognises the symbol
symbol = oneOf "!$%|*+-/:<=?>@^_~#"

spaces :: Parser () -- additonal features to recognize spaces
spaces = skipMany1 space

data LispVal = Atom String  --data type support for Scheme 
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer 
	| String String
	| Bool Bool
	


parseString :: Parser LispVal
parseString = do char '"'
				x <- many (noneOf "\"")
				char '"'
				return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol 
				rest < many (letter <|> digit <|>symbol)
				let atom = [first ] ++ rest 
				return $case atom of 
					"#t" -> Bool True
					"#f" -> Bool False
					otherwise -> Atom atom 



readExpr :: String -> String   -- function that parses data accordingt o "lisp" style
readExpr input = case parse (spaces >> symbol)  'lisp' input of 
	Left err -> "No Match : " ++ show err
	Right err -> "Found Value"


