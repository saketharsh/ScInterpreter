 ------------------------------------Parsing the Lisp Style Syntax ---------------------------
module SchemeParser where

import SchemeError
import Control.Monad (liftM)
import Control.Monad.Error
import Data.Array (Array (..), listArray)
import Data.Char (toLower)
import Data.Complex (Complex (..))
import Data.IORef
import Data.Ratio (Rational (..), (%))
import System.IO hiding (try)
import Numeric (readOct, readHex)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)




readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
	Left err ->  throwError $ Parser err
	Right val ->  return val


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
parseString = do
	char '"'
	s <- many (noneOf "\"")
	char '"'
	(return . String) s

parseNumber :: Parser LispVal
parseNumber = liftM ( Number . read ) $ many1 digit  -- to parse number

parseAtom :: Parser LispVal
parseAtom = do 
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
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
