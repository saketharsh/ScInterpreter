{-# LANGUAGE ExistentialQuantification #-}
module Main where

import LispVal
import SchemeParser
import SchemeEval
import Control.Monad.Error
import System.IO hiding (try)
import System.Environment

main :: IO ()  -- left to add eval function over the readExpr, in order to get the output
main = do
	args <- getArgs
	if null args then runRepl else runOne $ args


runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
      >>= hPutStrLn stderr

runRepl ::  IO ()
runRepl = primitiveBindings >>= until_ ( == "quit") (readPrompt "Scheme>>>") . evalAndPrint

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

