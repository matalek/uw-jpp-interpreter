module Checker(check) where

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


type Checker a = ReaderT Env (ErrorT String IO) a

getDataEnv :: Checker DataEnv
getDataEnv = do
  env <- ask
  return $ fst env

dataTypeOf :: Exp -> Checker DataType

dataTypeOf (EConst c) = case c of
  (EInt i) -> return Int
  ETrue -> return Bool
  EFalse -> return Bool

dataTypeOf (EVar v) = do
  env <- getDataEnv
  if member v env then return $ env ! v
    else throwError $ "Variable " ++ (show v) ++  "not in scope."


checkStmt :: Stmt -> Checker ()
checkStmt x = case x of
  SPrint printStmt -> checkPrintStmt printStmt
  _ -> return ()

checkPrintStmt :: PrintStmt -> Checker ()
checkPrintStmt (SPrintOne e) = do
  val <- dataTypeOf e
  return ()

checkFuncDef :: FunctionDef -> Checker Env
checkFuncDef (FuncParams _ _ (FuncBodyOne _ _ stmts _)) = do
  mapM checkStmt stmts
  ask

checkProgram :: Program -> Checker ()
checkProgram (Progr ds) = do
  env <- checkExternalDeclarations ds
  return ()
  

checkExternalDeclaration :: ExternalDeclaration -> Checker Env
checkExternalDeclaration x =  case x of
  Afunc functiondef -> checkFuncDef functiondef
  _ -> ask 
  
checkExternalDeclarations :: [ExternalDeclaration] -> Checker Env
checkExternalDeclarations [] = ask
checkExternalDeclarations (d:ds) = do
  newEnv1 <- checkExternalDeclaration d
  local (\_ -> newEnv1) $ checkExternalDeclarations ds

check :: Program -> ErrorT String IO ()
check p = do
  runReaderT (checkProgram p) (empty, empty)
  return ()
