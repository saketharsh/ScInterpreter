module Main where 
import Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces) -- defined our own spaces 


main :: IO ()
main = do 
	args <- getArgs
	putStrLn (readExpr (args !! 0))


symbol :: Parser Char   -- parser that recognises the symbol
symbol = oneOf "!$%|*+-/:<=?>@^_~#"

spaces :: Parser () -- additonal features to recognize spaces
spaces = skipMany1 space

readExpr :: String -> String 
readExpr input = case parse parseExpr "lisp" input of 
	Left err -> "No Match: " ++ show err
	Right err -> "Found Value"

data LispVal = Atom String  --data type support for Scheme 
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer 
	| String String
	| Bool Bool

parseString :: Parser LispVal
parseString = do 
	char '"'
	x <- many (noneOf "\"")
	char '"'
	return $ String x

parseNumber :: Parser LispVal
parseNumber = liftM (Number.read) $ many1 digit

parseAtom :: Parser LispVal
parseAtom = do 
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = [first ] ++ rest
	return $ case atom of 
		"#t" -> Bool True
		"#f" -> Bool False 
		otherwise -> Atom atom

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber




