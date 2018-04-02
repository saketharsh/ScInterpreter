module Main where

import Data.Char 
import Data.Array
import Control.Monad
import Control.Monad.Error -- for error handling in Lisp 
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces) -- defined our own spaces 

main :: IO ()  -- left to add eval function over the readExpr, in order to get the output
main = do 
	args <- getArgs
	evaled <- return $ liftM show $ readExpr (args !! 0 ) >>= eval
	putStrLn $ extractValue $ trapError evaled

 ------------------------------------Parsing the Lisp Style Syntax ---------------------------

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

---------------------------- Evaluating the Parsed Values -----------------------------------------------------------


instance Show LispVal where show = showVal -- helps us to print LispVal type values


showVal :: LispVal -> String   -- the basic printing function of our interpreter
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name ) = name
showVal (Number contents) = show contents
showVal (Bool True ) = "#t"
showVal (Bool False ) = "#f"
showVal (List contents ) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++  showVal tail ++ ")"

unwordsList :: [LispVal] -> String -- helper function to print lists in the interpreter 
unwordsList = unwords . map  showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _ ) =return  val
eval val@(Number _ ) = return val
eval val@(Bool _) = return val
eval ( List [Atom "quote" , val] ) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special Form"  badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe ( throwError $ NotFunction " Unrecognized primitive function args" func ) ($ args) $ (lookup func primitives)


primitives :: [(String, [LispVal]-> ThrowsError LispVal)] -- primitive operations that we wish to support1
primitives =   [( "+" , numericBinop (+) ),
				--( "-" , numericBinop (−) ),  -- - and * are crearting some problem, comment them out and evertything is fine
				--( "*" , numericBinop (∗) ),
				( "/" , numericBinop div ),
				( "mod" , numericBinop mod),
				( "quotient" , numericBinop quot),
				( "remainder" , numericBinop rem)]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal]-> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1  op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n ) = return n 
unpackNum (String n) = let parsed = reads n in 
							if null parsed
								then throwError $ TypeMismatch "number" $ String n 
								else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum  = throwError $ TypeMismatch "number" notNum

--------------------------------------Error Handling for Scheme -----------------------------------------

instance Show LispError where show = showError

instance Error LispError where 
	noMsg = Default "An error has occured"
	strMsg = Default

type ThrowsError = Either LispError   -- to return either the error, or the Lisp data type that we declared earlier

trapError action = catchError action (return . show )

extractValue :: ThrowsError a -> a 
extractValue (Right val ) = val

data LispError = NumArgs Integer [LispVal]  -- more support can be added as time and knowledge permits 
	| TypeMismatch String LispVal
	| Parser ParseError
	| BadSpecialForm String LispVal
	| NotFunction String String
	| UnboundVar String String
	| Default String


showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found ) = "Expected " ++ show expected ++ "args :: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found : " ++ show found 
showError (Parser parseError ) = "Parse error at " ++ show parseError
