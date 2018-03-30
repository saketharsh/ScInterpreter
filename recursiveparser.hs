module Main where 
import Control.Monad
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
	s <- many (noneOf "\"")
	char '"'
	(return . String) s 

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read   -- to parse number 

parseAtom :: Parser LispVal  -- to parse atoms 
parseAtom = do 
	first <- letter <|> symbol
	rest <- many ( letter <|> digit <|> symbol )
	let atom = [first] ++ rest
	return $ case atom of 
		"#t" -> Bool True
		"#f" -> Bool False 
		otherwise -> Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal  -- definitions to define special characters inside the quotes
parseDottedList = do
	head <- endBy parseExpr spaces
	tail <- char '.' >> spaces >> parseExpr
	return $ DottedList head tail

parseQuoted :: Parser LispVal   -- defintion to allow nested quotes"
parseQuoted = do 
	char '\''
	x <- parseExpr
	return $ List [ Atom "quote", x]

parseExpr :: Parser LispVal -- to parse according to data type  Here the order in which I have mentioned things matter 
parseExpr = parseAtom 
	<|> parseString 
	<|> parseNumber 
	<|> parseQuoted
	<|> do
		char '('
		x <- (try parseList) <|> parseDottedList
		char ')'
		return x







