{-# LANGUAGE ExistentialQuantification #-}
module Main where

import LispVal
import SchemeParser
import SchemeEval
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
		1 -> runOne $ (args !! 0)
		otherwise -> putStrLn "Give suitable or no input"


runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl ::  IO ()
runRepl = nullEnv >>= until_ ( == "quit") (readPrompt "Scheme>>>") . evalAndPrint




-------------------------------------Helper IO functions----------------------------------------------------

flushStr :: String -> IO ()  -- function to take input and immediately flush out the result that we have evaluated
flushStr str = putStr str >> hFlush stdout


readPrompt :: String -> IO String  -- simple prompt generator for the scheme 
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint ::  Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool ) -> m a -> (a -> m () ) -> m ()   -- Monadic gunction to help us come out of the Scheme 
until_ pred prompt action = do 
	result <- prompt
	if pred result
		then return ()
		else action result >> until_ pred prompt action

