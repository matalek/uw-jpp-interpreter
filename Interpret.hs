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
  
main = do
  input <- getContents
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
    (Bad s) -> hPutStrLn stderr s

exec :: String -> ExceptT String IO ()
exec input = 
  let Ok s = pProgram (myLexer input) 
  in interpret s
