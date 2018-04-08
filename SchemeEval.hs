{-# LANGUAGE ExistentialQuantification #-}
---------------------------- Evaluating the Parsed Values -----------------------------------------------------------
module SchemeEval where 

import LispVal
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



eval :: Env ->  LispVal -> IOThrowsError LispVal
eval env val@(String _ ) =return  val
eval env val@(Number _ ) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env ( List [Atom "quote" , val] ) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
	result <- eval env pred
	case result of
		Bool False -> eval env  alt
		Bool False  -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var , form]) = eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env )args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special Form"  badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe ( throwError $ NotFunction " Unrecognized primitive function args" func ) ($ args) $ (lookup func primitives)


primitives :: [(String, [LispVal]-> ThrowsError LispVal)] -- primitive operations that we wish to support1
primitives =   [( "+" , numericBinop (+) ),
				( "-" , numericBinop minuss ),  -- - and * are crearting some problem, comment them out and evertything is fine
				( "*" , numericBinop intoo ),
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


minuss :: Integer -> Integer -> Integer
minuss a b  = a - b

intoo :: Integer -> Integer -> Integer
intoo a b = a*b


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
