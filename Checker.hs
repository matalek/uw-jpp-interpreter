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

data DataType = Int | Bool | Void | Array DataType | Map DataType DataType deriving (Eq, Show)
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

setFuncType :: FName -> FuncType -> Checker Env
setFuncType v t = do
  dataEnv <- getDataEnv
  funcEnv <- getFuncEnv
  return (dataEnv, insert v t funcEnv)

toDataType :: TypeSpecifier -> DataType
toDataType TInt = Int
toDataType TBool = Bool
toDataType TVoid = Void
toDataType (TArray t) = Array $ toDataType t
toDataType (TMap t1 t2) = Map (toDataType t2) (toDataType t1) -- keys and values in different order

funcTypeOf :: Exp -> Checker FuncType
funcTypeOf (EVar f) = do
  env <- getFuncEnv
  if member f env then return $ env ! f
    else  lift $ throwE $ "Function " ++ (showVar f) ++  " not in scope."

-- ???????
funcTypeOf _ = lift $ throwE "Some kind of error"

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

dataTypeOf (EFunk f) = dataTypeOf (EFunkPar f [])
  
dataTypeOf (EFunkPar f exps) = do
  argumentsType <- mapM dataTypeOf exps
  fun <- funcTypeOf f 
  if snd fun == argumentsType then return $ fst fun
    else if length (snd fun) == length argumentsType then lift $ throwE "Function arguments do not have correct type"
         else lift $ throwE "Number of function arguments is wrong"

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

dataTypeOf x = do
  lift $ throwE $ "Nothing for: " ++ (show x)

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


checkFuncDefs :: [FunctionDef] -> Checker Env
checkFuncDefs [] = ask
checkFuncDefs (f:fs) = do
  newEnv1 <- checkFuncDef f
  local (\_ -> newEnv1) $ checkFuncDefs fs

checkFuncDef :: FunctionDef -> Checker Env
checkFuncDef (FuncParams (DVariable returnType funName) params (FuncBodyOne ds fs stmts es)) = do
  let paramsDec = Prelude.map (\ x -> (Declaration x)) params
  env1 <- checkDec paramsDec -- params
  env2 <- local (\_ -> env1) $ checkDec ds -- declarations
  let paramsType = Prelude.map (\(DVariable paramType _) -> toDataType paramType) params 
  let funcType = (toDataType returnType, paramsType)
  env3 <- local (\_ -> env2) $ setFuncType funName funcType -- recursion
  env4 <- local (\_ -> env3) $ checkFuncDefs fs -- inner functions
  local (\_ -> env4) $ mapM checkStmt stmts
  let ret1 = toDataType returnType 
  ret2 <- local (\_ -> env4) $ dataTypeOfStmt es
  if ret1 /= ret2 then lift $ throwE "Return type is not correct"
    else setFuncType funName funcType

checkProgram :: Program -> Checker ()
checkProgram (Progr ds) = do
  env <- checkExternalDeclarations ds
  return ()
  

checkExternalDeclaration :: ExternalDeclaration -> Checker Env
checkExternalDeclaration x =  case x of
  Afunc functiondef -> checkFuncDef functiondef
  Global dec -> checkDec [dec]
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
