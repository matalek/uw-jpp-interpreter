module Main where
  
import Lexmatal
import Parmatal
import Absmatal
import Interpreter
  
import ErrM
  
main = do
  input <- getContents
  exec input
  putStrLn ""
  
exec input = 
  let Ok s = pStm (myLexer input) 
  in interpret s
