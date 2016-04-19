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
type SName = Ident

data DataType = Int | Bool | Void | Array DataType | Map DataType DataType
              | Structt SName deriving (Eq, Show)
type FuncType = (DataType, [DataType])
type StructType = Map Var DataType

type DataEnv = Map Var DataType
type FuncEnv = Map FName FuncType
type StructEnv = Map SName StructType

type Env = (DataEnv, FuncEnv, StructEnv)

type Result = ExceptT String IO

type Checker a = ReaderT Env Result a

showVar :: Var -> String
showVar (Ident s) = s

getDataEnv :: Checker DataEnv
getDataEnv = do
  (env, _, _) <- ask
  return env

getFuncEnv :: Checker FuncEnv
getFuncEnv = do
  (_, env, _) <- ask
  return env

getStructEnv :: Checker StructEnv
getStructEnv =  do
  (_, _, env) <- ask
  return env

setDataType :: Var -> DataType -> Checker Env
setDataType v t = do
  (dataEnv, funcEnv, structEnv) <- ask
  return (insert v t dataEnv, funcEnv, structEnv)

setFuncType :: FName -> FuncType -> Checker Env
setFuncType v t = do
  (dataEnv, funcEnv, structEnv) <- ask
  return (dataEnv, insert v t funcEnv, structEnv)

toDataType :: TypeSpecifier -> DataType
toDataType TInt = Int
toDataType TBool = Bool
toDataType TVoid = Void
toDataType (TArray t) = Array $ toDataType t
toDataType (TMap t1 t2) = Map (toDataType t2) (toDataType t1) -- keys and values in different order 
toDataType (TStruct v) = Structt v

funcTypeOf :: Exp -> Checker FuncType
funcTypeOf (EVar f) = do
  env <- getFuncEnv
  if member f env then return $ env ! f
    else  lift $ throwE $ "Function " ++ (showVar f) ++  " not in scope."

-- ???????
funcTypeOf _ = lift $ throwE "Some kind of error"

structTypeOf :: Exp -> Checker StructType
structTypeOf s = do
  s' <- dataTypeOf s
  env <- getStructEnv
  case s'  of
    (Structt struct) -> do
      if member struct env then return $ env ! struct
        else lift $ throwE $ "Struct " ++ showVar struct ++ " not defined"
    _ -> lift $ throwE "Not a struct"

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
  let lValue = isLValue e1
  type1 <- dataTypeOf e1
  type2 <- dataTypeOf e2
  if not lValue then lift $ throwE "Not a l-value on the left side of an assignment"
    else if type1 /= type2 then lift $ throwE "Assign expressions with different types"
         else return type1
              
dataTypeOf (EPlus e1 e2) = checkBinOpInt e1 e2
dataTypeOf (EMinus e1 e2) = checkBinOpInt e1 e2
dataTypeOf (ETimes e1 e2) = checkBinOpInt e1 e2
dataTypeOf (EDiv e1 e2) = checkBinOpInt e1 e2

dataTypeOf (EPostInc e) = dataTypeOf $ EAssign e AssignAdd (EConst $ EInt 1)
dataTypeOf (EPostDec e) = dataTypeOf $ EAssign e AssignSub (EConst $ EInt 1)

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

dataTypeOf (ESelect s field) = do
  struct <- structTypeOf s
  if member field struct then return $ struct ! field
    else lift $ throwE "Struct has no such field"
         
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

-- Check, whether given expression can be on the left side of the assignment operator.
-- We only accept single-level assignment (you cannot for example assign a field for an
-- object in an array).
isLValue :: Exp -> Bool 
isLValue (EVar _) = True
isLValue (EArray (EVar _) _) = True
isLValue (EMap (EVar _) _) = True
isLValue (ESelect (EVar _) _) = True
isLValue _ = False

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

checkStructSpec :: StructSpec -> Checker Env
checkStructSpec (Struct sname ds) = do
  let set cur (Declaration (DVariable t v)) =
        insert v (toDataType t) cur
  let struct = Prelude.foldl set empty ds
  (dataEnv, funcEnv, structEnv) <- ask
  return (dataEnv, funcEnv, insert sname struct structEnv) 

checkProgram :: Program -> Checker ()
checkProgram (Progr ds) = do
  env <- checkExternalDeclarations ds
  return ()

checkExternalDeclaration :: ExternalDeclaration -> Checker Env
checkExternalDeclaration x =  case x of
  Afunc functiondef -> checkFuncDef functiondef
  Global dec -> checkDec [dec]
  StructDec structspec -> checkStructSpec structspec
  
checkExternalDeclarations :: [ExternalDeclaration] -> Checker Env
checkExternalDeclarations [] = ask
checkExternalDeclarations (d:ds) = do
  newEnv1 <- checkExternalDeclaration d
  local (\_ -> newEnv1) $ checkExternalDeclarations ds

check :: Program -> Result ()
check p = do
  runReaderT (checkProgram p) (empty, empty, empty)
  return ()
