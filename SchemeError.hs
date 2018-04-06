
--------------------------------------Error Handling for Scheme -----------------------------------------
module SchemeError where

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



