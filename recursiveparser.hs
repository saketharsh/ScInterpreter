module Main where 
import Control.Monad
import Control.Monad.Error
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces) -- defined our own spaces 

main :: IO ()  -- left to add eval function over the readExpr, in order to get the output
main = do 
	args <- getArgs
	putStrLn ( (show . eval . readExpr) (args !! 0))

 ------------------------------------Parsing the Lisp Style Syntax ---------------------------

symbol :: Parser Char   -- parser that recognises the symbol
symbol = oneOf "!$%|*+-/:<=?>@^_~#"

spaces :: Parser () -- additonal features to recognize spaces
spaces = skipMany1 space

readExpr :: String -> LispVal  -- change the data type to String -> LispVal , coz we will evaluate the output from thid function
readExpr input = case parse parseExpr "lisp" input of 
	Left err -> String $ "No Match: " ++ show err
	Right val ->  val

data LispVal = Atom String  --data type support for Scheme 
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer 
	| String String
	| Bool Bool

data LispError = NumArgs Integer [LispVal]
	| TypeMismatch String LispVal
	| Parser ParseError
	| BadSpeciaForm String LispVal
	| NotFunction String String
	| Unboundvar String String
	| Default String

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
instance Show LispError where show = showError

instance Error LispError where 
	noMsg = Default "An error has occured"
	strMsg = Default

unwordsList :: [LispVal] -> String -- helper function to print lists in the interpreter 
unwordsList = unwords . map  showVal

type ThrowsError = Either LispError

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm messafe form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found ) = "Expected " ++ show expected ++ "args :: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found : " ++ show found 
showError (Parser parseError ) = "Parse error at " ++ show parseErr


showVal :: LispVal -> String   -- the basic printing function of our interpreter
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name ) = name
showVal (Number contents) = show contents
showVal (Bool True ) = "#t"
showVal (Bool False ) = "#f"
showVal (List contents ) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++  showVal tail ++ ")"



eval :: LispVal -> LispVal
eval val@(String _ ) = val
eval val@(Number _ ) = val
eval val@(Bool _) = val
eval ( List [Atom "quote" , val] ) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe ( Bool False) ($ args) $ lookup func primitives


primitives :: [(String, [LispVal]-> LispVal)] -- primitive operations that we wish to support1
primitives =   [( "+" , numericBinop (+) ),
				( "-" , numericBinop (−) ),  -- - and * are crearting some problem, comment them out and evertything is fine
				( "*" , numericBinop (∗) ),
				( "/" , numericBinop div ),
				( "mod" , numericBinop mod),
				( "quotient" , numericBinop quot),
				( "remainder" , numericBinop rem)]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal]-> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n ) = n 
unpackNum (String n) = let parsed = reads n in 
							if null parsed
								then 0
								else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

