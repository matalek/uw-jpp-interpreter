-- Main file for Interpreter
-- Aleksander Matusiak

module Main where
  
import LexMatal
import ParMatal
import AbsMatal
import Interpreter
import System.IO
import Checker
       
import ErrM
import Control.Monad.Except
import Control.Monad.Trans.Except
import System.Environment
       
main = do
  args <- getArgs
  input <- case args of
    [] -> getContents
    (file:_) -> readFile file
  case pProgram (myLexer input) of
    (Ok s) -> do
      analysis <- runExceptT $ check s
      case analysis of
        (Left e) -> hPutStrLn stderr $ "Type error: " ++ e
        _ -> do
          res <- runExceptT $ interpret s
          case res of
            (Left e) -> hPutStrLn stderr $ "Runtime error: " ++ e
            _ -> return ()
    (Bad s) -> hPutStrLn stderr s -- syntax error
