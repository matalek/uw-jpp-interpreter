module Interpreter where

import Absmatal
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Map

type Var = String
type Loc = Int
data Val = Int Int
type Store = Map Loc Val
type Env = Map Var Loc

type Interpreter a = StateT Store (ReaderT Env IO) a

getLoc :: Var -> Interpreter Loc
getLoc v = do
  env <- ask
  return $ env ! v

getVal :: Var -> Interpreter Val
getVal v = do
  store <- get
  loc <- getLoc v
  return $ store ! loc

alloc :: Interpreter Loc
alloc = do
  store <- get
  let loc = if size store /= 0 then (fst $ findMax store) else 0 + 1
  put $ insert loc (Int 0) store
  return loc


-- Right now only for statements
interpret :: Stm -> IO ()
interpret s = do
  runReaderT (execStateT (transStm s) empty) empty
  return ()

transStm :: Stm -> Interpreter ()
transStm (PrintS (Sprint (Econst (Eint i)))) = do
             lift $ lift $ print i


transExp :: Exp -> Interpreter Val
transExp _ = error ""
