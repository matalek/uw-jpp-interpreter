module Checker where

import AbsMatal
import ErrM
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import Data.Map

type Var = Ident
type FName = Ident

data DataType = Int | Bool | Void | Array DataType | Map DataType DataType
type FuncType = (DataType, [DataType])

type DataEnv = Map Var DataType
type FuncEnv = Map FName FuncType

type Env = (DataEnv, FuncEnv)


type Checker a = ReaderT Env (ErrorT String Identity) a


getDataEnv :: Checker DataEnv
getDataEnv = do
  env <- ask
  return $ fst env

evalDataType :: Exp -> Checker DataType

evalDataType (EConst c) = case c of
  (EInt i) -> return Int
  ETrue -> return Bool
  EFalse -> return Bool

evalDataType (EVar v) = do
  env <- getDataEnv
  if member v env then return $ env ! v
    else throwError $ "Variable " ++ (show v) ++  "not in scope."
