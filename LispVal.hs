{-# LANGUAGE ExistentialQuantification #-}
--------------------------------------Error Handling for Scheme -----------------------------------------
module LispVal where

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


data LispVal = Atom String  --data type support for Scheme
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer
	| String String
	| Bool Bool



data LispError = NumArgs Integer [LispVal]  -- more support can be added as time and knowledge permits
	| TypeMismatch String LispVal
	| Parser ParseError
	| BadSpecialForm String LispVal
	| NotFunction String String
	| UnboundVar String String
	| Default String


instance Show LispVal where show = showVal -- helps us to print LispVal type values

unwordsList :: [LispVal] -> String -- helper function to print lists in the interpreter
unwordsList = unwords . map  showVal

showVal :: LispVal -> String   -- the basic printing function of our interpreter
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name ) = name
showVal (Number contents) = show contents
showVal (Bool True ) = "#t"
showVal (Bool False ) = "#f"
showVal (List contents ) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++  showVal tail ++ ")"

instance Show LispError where show = showError

instance Error LispError where
	noMsg = Default "An error has occured"
	strMsg = Default


type ThrowsError = Either LispError   -- to return either the error, or the Lisp data type that we declared earlier

trapError action = catchError action (return . show )

extractValue :: ThrowsError a -> a
extractValue (Right val ) = val



showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found ) = "Expected " ++ show expected ++ "args :: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found : " ++ show found
showError (Parser parseError ) = "Parse error at " ++ show parseError

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a 
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


runIOThrows :: IOThrowsError String -> IO String 
runIOThrows action = runErrorT ( trapError action) >>= return . extractValue


isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True ). lookup var


getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
	env <- liftIO $ readIORef envRef
	maybe (throwError $ UnboundVar " Getting an Unbound variable" var) (liftIO . readIORef) (lookup var env)


setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
	env <- liftIO $ readIORef envRef 
	maybe   ( throwError $ UnboundVar "Setting an Unbound Variable" var) 
			(liftIO . (flip writeIORef value)) 
			(lookup var env)
	return value


defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
       then setVar envRef var value >> return value
       else liftIO $ do
           valueRef <- newIORef value
           env <- readIORef envRef
           writeIORef envRef ((var, valueRef) : env)
           return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do
            ref <- newIORef value
            return (var, ref)

