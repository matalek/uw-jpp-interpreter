module Checker(check) where

import AbsMatal
import ErrM
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Identity
import Data.Map

type Var = Ident
type FName = Ident

data DataType = Int | Bool | Void | Array DataType | Map DataType DataType deriving (Eq)
type FuncType = (DataType, [DataType])

type DataEnv = Map Var DataType
type FuncEnv = Map FName FuncType

type Env = (DataEnv, FuncEnv)

type Result = ExceptT String IO

type Checker a = ReaderT Env Result a

showVar :: Var -> String
showVar (Ident s) = s

getDataEnv :: Checker DataEnv
getDataEnv = do
  env <- ask
  return $ fst env

getFuncEnv :: Checker FuncEnv
getFuncEnv = do
  env <- ask
  return $ snd env


setDataType :: Var -> DataType -> Checker Env
setDataType v t = do
  dataEnv <- getDataEnv
  funcEnv <- getFuncEnv
  return (insert v t dataEnv, funcEnv)

toDataType :: TypeSpecifier -> DataType
toDataType TInt = Int
toDataType TBool = Bool

dataTypeOf :: Exp -> Checker DataType

dataTypeOf (EConst c) = case c of
  (EInt i) -> return Int
  ETrue -> return Bool
  EFalse -> return Bool

dataTypeOf (EVar v) = do
  env <- getDataEnv
  if member v env then return $ env ! v
    else lift $ throwE $ "Variable " ++ (showVar v) ++  " not in scope."

-- very easy right now, will have to add lvalues
dataTypeOf (EAssign e1 _ e2) = do
  type1 <- dataTypeOf e1
  type2 <- dataTypeOf e2
  if type1 == type2 then return type1
    else lift $ throwE "Assign expressions with different types"

dataTypeOf (EPlus e1 e2) = checkBinOpInt e1 e2
dataTypeOf (EMinus e1 e2) = checkBinOpInt e1 e2
dataTypeOf (ETimes e1 e2) = checkBinOpInt e1 e2
dataTypeOf (EDiv e1 e2) = checkBinOpInt e1 e2

dataTypeOf (ELthen e1 e2) = checkBinOpBool e1 e2
dataTypeOf (EGrthen e1 e2) = checkBinOpBool e1 e2
dataTypeOf (ELe e1 e2) = checkBinOpBool e1 e2
dataTypeOf (EGe e1 e2) = checkBinOpBool e1 e2

{-
dataTypeOf (EFunk (EVar f)) = do
  (Fun fun) <- getFun f
  fun []
dataTypeOf (EFunkPar (EVar f) exps) = do
  arguments <- mapM dataTypeOf exps
  (Fun fun) <- getFun f
  fun arguments
-}

dataTypeOf (EArray v exp) = do
  type1 <- dataTypeOf v
  type2 <- dataTypeOf exp
  case type1 of
    (Array valueType) -> do
      if type2 == Int then  return valueType
        else lift $ throwE "Index is not integer"
    _ -> lift $ throwE "Trying to get element of not array"

dataTypeOf (EMap v exp) = do
  type1 <- dataTypeOf v
  type2 <- dataTypeOf exp
  case type1 of
    (Map keyType valueType) -> do
      if type2 == keyType then return valueType
        else lift $ throwE "Map key has not an appropriate type"
    _ -> lift $ throwE "Trying to get element of not map"

checkBinOpInt :: Exp -> Exp -> Checker DataType
checkBinOpInt e1 e2 = do
  type1 <- dataTypeOf e1
  type2 <- dataTypeOf e2
  if type1 == Int && type2 == Int then return Int
    else lift $ throwE "Binary operator applied not to integers"

checkBinOpBool :: Exp -> Exp -> Checker DataType
checkBinOpBool e1 e2 = do
  type1 <- dataTypeOf e1
  type2 <- dataTypeOf e2
  if type1 == Int && type2 == Int then return Bool
    else lift $ throwE "Compare operator applied not to integers"


skip :: Stmt
skip = SComp (SCompOne [] [])

checkStmts :: [Stmt] -> Checker ()
checkStmts ss = do
  mapM checkStmt ss
  return ()

checkStmt :: Stmt -> Checker ()
checkStmt x = case x of
  SComp compoundStmt  -> checkCompoundStmt compoundStmt
  SExpr expressionStmt  -> checkExpressionStmt expressionStmt
  SSel selectionStmt  -> checkSelectionStmt selectionStmt
  SIter iterStmt  -> checkIterStmt iterStmt
  SPrint printStmt  -> checkPrintStmt printStmt
  SInit initStmt  -> checkInitStmt initStmt

checkExpressionStmt :: ExpressionStmt -> Checker ()
checkExpressionStmt SExprOne = return ()
checkExpressionStmt (SExprTwo e) = do
  _ <- dataTypeOf e
  return ()

checkSelectionStmt :: SelectionStmt -> Checker ()
checkSelectionStmt (SSelOne e s) =
  checkSelectionStmt (SSelTwo e s skip) 
checkSelectionStmt (SSelTwo e s1 s2) = do
  cond <- dataTypeOf e
  if cond == Bool then do
    checkStmt s1
    checkStmt s2
  else lift $ throwE "The type of condition is not bool"

checkIterStmt :: IterStmt -> Checker ()
checkIterStmt (SIterOne e s) = checkSelectionStmt (SSelOne e s)         

checkIterStmt (SIterTwo es1 es2 s) =
  checkIterStmt (SIterThree es1 es2 (EConst ETrue) s)

checkIterStmt (SIterThree es1 es2 e s) = do
  _ <- dataTypeOfStmt es1
  cond <- dataTypeOfStmt es2
  _ <- dataTypeOf e
  if cond == Bool then checkStmt s
    else lift $ throwE "The type of condition is not bool"

dataTypeOfStmt :: ExpressionStmt -> Checker DataType
dataTypeOfStmt SExprOne = return Void -- don't know ???
dataTypeOfStmt (SExprTwo e) = dataTypeOf e

checkCompoundStmt :: CompoundStmt -> Checker ()
checkCompoundStmt (SCompOne ds ss) = do
  newEnv <- checkDec ds
  local (\_ -> newEnv) $ checkStmts ss

checkInitStmt :: InitStmt -> Checker ()
checkInitStmt (SInitOne v e) = do
  _ <- dataTypeOf (EVar v) -- check if v was declared
  type2 <- dataTypeOf e
  if type2 == Int then return ()
    else lift $ throwE "Size of array is not an integer" 

checkPrintStmt :: PrintStmt -> Checker ()
checkPrintStmt (SPrintOne e) = do
  val <- dataTypeOf e
  return ()

checkDec :: [Dec] -> Checker Env
checkDec [] = ask
checkDec ((Declaration (DVariable t v)):ds) = do
  env <- setDataType v $ toDataType t
  local (\_ -> env) $ checkDec ds

checkFuncDef :: FunctionDef -> Checker Env
checkFuncDef (FuncParams _ _ (FuncBodyOne ds _ stmts _)) = do
  env <- checkDec ds
  local (\_ -> env) $ mapM checkStmt stmts
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

check :: Program -> Result ()
check p = do
  runReaderT (checkProgram p) (empty, empty)
  return ()
