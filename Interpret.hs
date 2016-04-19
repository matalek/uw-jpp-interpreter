module Main where
  
import LexMatal
import ParMatal
import AbsMatal
import Interpreter
import System.IO
import Checker
       
import ErrM
import Control.Monad.Error
  
main = do
  input <- getContents
  let Ok s = pProgram (myLexer input)
  analysis <- runErrorT $ check s
  case analysis of
    (Left e) -> hPutStrLn stderr $ "Type error: " ++ e
    _ -> do
      res <- runErrorT $ interpret s
      case res of
        (Left e) -> hPutStrLn stderr e
        _ -> return ()

exec :: String -> ErrorT String IO ()
exec input = 
  let Ok s = pProgram (myLexer input) 
  in interpret s
