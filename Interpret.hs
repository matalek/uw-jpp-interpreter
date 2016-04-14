module Main where
  
import LexMatal
import ParMatal
import AbsMatal
import Interpreter
  
import ErrM
  
main = do
  input <- getContents
  exec input
  putStrLn ""
  
exec input = 
  let Ok s = pStmt (myLexer input) 
  in interpret s
