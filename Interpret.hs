module Main where
  
import LexMatal
import ParMatal
import AbsMatal
import Interpreter
import System.IO
  
import ErrM
import Control.Monad.Error
  
main = do
  input <- getContents
  res <- runErrorT $ exec input
  case res of
    (Left e) -> hPutStrLn stderr e
    _ -> return ()

exec :: String -> ErrorT String IO ()
exec input = 
  let Ok s = pProgram (myLexer input) 
  in interpret s
