{-# LANGUAGE ExistentialQuantification #-}
module Main where

import SchemeEval
import SchemeParser
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

main :: IO ()  -- left to add eval function over the readExpr, in order to get the output
main = do
	args <- getArgs
	case length args of
		0 -> runRepl
		1 -> evalAndPrint $ (args !! 0)
		otherwise -> putStrLn "Give suitable or no input"


runRepl :: IO ()
runRepl = until_ ( == "quit") (readPrompt "Scheme>>>") evalAndPrint

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
eval (List [Atom "if", pred, conseq, alt]) = do
	result <- eval pred
	case result of
		Bool False -> eval alt
		Bool False  -> eval conseq
		othewise -> throwError $ TypeMismatch "boolean" result
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
				( "remainder" , numericBinop rem),
				("=", numBoolBinop (==) ),
				("<" , numBoolBinop (<)),
				(">", numBoolBinop (>)),
				("/=", numBoolBinop (/=)),
				(">=", numBoolBinop (>=)),
				("<=", numBoolBinop (<=)),
				("&&", boolBoolBinop (&&)),
				("||", boolBoolBinop (||)),
				("string=?", strBoolBinop (==)),
				("string?", strBoolBinop (>)),
				("cdr", cdr), -- this is something you want to look at
				("car", car),
				("cons", cons),
				("eq?", eqv),
				("eqv?", eqv),
				("equal?", equal),
				("string<?", strBoolBinop (<=)),
				("string>?", strBoolBinop (>=))]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal]-> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1  op


boolBinop :: (LispVal -> ThrowsError a)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> ThrowsError LispVal
boolBinop unpacker op [x,y] = do
    left <- unpacker x
    right <- unpacker y
    return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s ) = return s
unpackStr (Number s ) = return $ show s
unpackStr (Bool s ) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string " notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b ) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n ) = return n
unpackNum (String n) = let parsed = reads n in
							if null parsed
								then throwError $ TypeMismatch "number" $ String n
								else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum  = throwError $ TypeMismatch "number" notNum


---------------------------------------List Manipulations in Scheme -------------------------------------

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List [] ] = return $ List [x1]
cons [x1, List xs] = return $ List $ [x1] ++ xs
cons [ x , DottedList xs xlast ] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList





--------------------------------------Equality Checks----------------------------------------------------


eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool b1), (Bool b2)] = (return . Bool) $ b1 == b2
eqv [(Number n1), (Number n2)] = (return . Bool) $ n1 == n2
eqv [(String s1), (String s2)] = (return . Bool) $ s1 == s2
eqv [(Atom a1), (Atom a2)] = (return . Bool) $ a1 == a2

eqv [(DottedList xs x), (DottedList ys y)] =
    eqv [List $ xs ++ [x], List $ ys ++ [y]]

eqv [(List l1), (List l2)]
    | length l1 /= length l2 = return $ Bool False
    | otherwise = (return . Bool) $ all byPairs $ zip l1 l2
  where byPairs (x,y) = case eqv [x,y] of
                             Left err -> False
                             Right (Bool val) -> val

eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList



data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)  -- to handle some extra equality conditions supported in scheme 


unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2  (AnyUnpacker unpacker ) = do
	unpacked1 <- unpacker arg1
	unpacked2 <- unpacker arg2
	return $ unpacked1 == unpacked2
	`catchError` (const $ return False)


equal ::[LispVal] -> ThrowsError LispVal  -- basically handles 2 == "2" -> "True" case   
equal [arg1, arg2] = do
	primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
		[AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
	eqvEquals <- eqv [arg1, arg2]
	return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList



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




-------------------------------------Helper IO functions----------------------------------------------------

flushStr :: String -> IO ()  -- function to take input and immediately flush out the result that we have evaluated
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String  -- simple prompt generator for the scheme 
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr  expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool ) -> m a -> (a -> m () ) -> m ()   -- Monadic gunction to help us come out of the Scheme 
until_ pred prompt action = do 
	result <- prompt
	if pred result
		then return ()
		else action result >> until_ pred prompt action





-- ------------------------------------------- Defining Variable in Scheme -----------------------------------------------------


-- type Env = IORef [(String , IORef LispVal)]
-- type IOThrowsError = ErrorT LispError IO

-- nullEnv :: IO Env
-- nullEnv = newIORef []

-- liftThrows :: ThrowError a -> IoThrowsError a 
-- liftThrows (Left err) = throwError err
-- liftThrows (Right val) = return val
