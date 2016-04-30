module Interpreter where

import AbsMatal
import PrintMatal
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Map
import ErrM

type Var = Ident
type FName = Ident
type Loc = Int
data Val = Int Int | Bool Bool | Array Int (Map Int Loc) | Mapp (Map Val Loc) | Structt (Map Ident Loc) | Null deriving (Show, Eq, Ord)
newtype Fun = Fun ([Val] -> Interpreter Val)

type Store = Map Loc Val
type Env = (Map Var Loc, Map FName Fun)

type Result = ExceptT String IO

type Interpreter a = StateT Store (ReaderT Env Result) a

getVarLoc :: Var -> Interpreter Loc
getVarLoc v = do
  env <- ask
  return $ fst env ! v

setLoc :: Var -> Loc -> Env -> Env
setLoc v loc (eVar, eFun) =
  let newEVar = insert v loc eVar
  in (newEVar, eFun)

setFun :: FName -> Fun -> Env -> Env
setFun name fun (eVar, eFun) = 
  let newEFun = insert name fun eFun
  in (eVar, newEFun)

getFun :: FName -> Interpreter Fun
getFun f = do
  env <- ask
  return $ snd env ! f 
  
getVarVal :: Var -> Interpreter Val
getVarVal v = do
  store <- get
  loc <- getVarLoc v
  return $ store ! loc

setVarVal :: Var -> Val -> Interpreter ()
setVarVal var val = do
  loc <- getVarLoc var
  modify $ insert loc val

getLocVal :: Loc -> Interpreter Val
getLocVal loc = do
  store <- get
  return $ store ! loc

showVal :: Val -> String
showVal (Int i) = show i
showVal (Bool b)
  | b = "true"
  | otherwise = "false"
showVal (Array _ _) = "array"
showVal (Mapp _) = "map"
showVal (Structt _) = "struct"
showVal Null = "null"

alloc :: Interpreter Loc
alloc = do
  store <- get
  let loc = if size store /= 0 then (fst $ findMax store) + 1 else 1
  put $ insert loc Null store -- the value is not important - it is overriden in declaration
  return loc

interpret :: Program -> (Result ())
interpret p = do
  runReaderT (execStateT (transProgram p) empty) (empty, empty)
  return ()

-- Statement execution
transStmts :: [Stmt] -> Interpreter ()
transStmts ss = do
  mapM transStmt ss
  return ()

transStmt :: Stmt -> Interpreter ()
transStmt x = case x of
  SComp compoundStmt  -> transCompoundStmt compoundStmt
  SExpr expressionStmt  -> transExpressionStmt expressionStmt
  SSel selectionStmt  -> transSelectionStmt selectionStmt
  SIter iterStmt  -> transIterStmt iterStmt
  SPrint printStmt  -> transPrintStmt printStmt
  SInit initStmt  -> transInitStmt initStmt

-- Expression statements
transExpressionStmt :: ExpressionStmt -> Interpreter ()
transExpressionStmt SExprOne = return ()
transExpressionStmt (SExprTwo e) = do
  _ <- transExp e
  return ()

-- Selection statements
transSelectionStmt :: SelectionStmt -> Interpreter ()
transSelectionStmt (SSelOne e s) =
  transSelectionStmt (SSelTwo e s (SCompOne [] [])) 
transSelectionStmt (SSelTwo e s1 s2) = do
  val <- transExp e
  case val of
    (Bool True) -> transCompoundStmt s1
    _ -> transCompoundStmt s2

-- Iter statements
transIterStmt :: IterStmt -> Interpreter ()
transIterStmt w@(SIterOne e s) = do
  val <- transExp e
  case val of
    (Bool True) -> transStmts [s, (SIter w)]
    _ -> return ()                          

transIterStmt (SIterTwo es1 es2 s) =
  transIterStmt (SIterThree es1 es2 (EConst ETrue) s)

transIterStmt (SIterThree es1 es2 e s) = do
  let cond =
        case es2 of
          SExprOne -> EConst ETrue
          (SExprTwo c) -> c
  let loop = SComp (SCompOne [] [s, (SExpr (SExprTwo e))])
  transStmts [(SExpr es1), (SIter (SIterOne cond loop))]

-- Print statements
transPrintStmt :: PrintStmt -> Interpreter ()
transPrintStmt (SPrintOne  e) = do
  val <- transExp e
  lift $ lift $ lift $ putStrLn $ showVal val

-- Compound statements
transCompoundStmt :: CompoundStmt -> Interpreter ()
transCompoundStmt (SCompOne ds ss) = do
  newEnv <- transDec ds
  local (const newEnv) $ transStmts ss

-- Init statements
transInitStmt :: InitStmt -> Interpreter ()
transInitStmt input@(SInitOne e1 e2) = do
  loc <- toLoc e1
  (Int n) <- transExp e2
  if n >= 0 then modify (insert loc (Array n Data.Map.empty))
    else lift $ lift $ throwE $ "An array cannot have negative size: " ++ printTree input
         
-- Expression evaluation
transExp :: Exp -> Interpreter Val

transExp (EConst c) = case c of
  (EInt i) -> return $ Int (fromInteger i)
  ETrue -> return $ Bool True
  EFalse -> return $ Bool False

transExp (EVar v) = getVarVal v

transExp (EAssign e1 op e2) = do
  newVal <-
    case op of
      Assign -> transExp e2
      AssignAdd -> transExp (EPlus e1 e2)
      AssignSub -> transExp (EMinus e1 e2)
      AssignMul -> transExp (ETimes e1 e2)
      AssignDiv -> transExp (EDiv e1 e2)
  assign e1 newVal
  return newVal

transExp (EPlus e1 e2) = evalBinOpInt e1 e2 (+)
transExp (EMinus e1 e2) = evalBinOpInt e1 e2 (-)
transExp (ETimes e1 e2) = evalBinOpInt e1 e2 (*)
transExp (EDiv e1 e2) = do
  (Int val1) <- transExp e1
  (Int val2) <- transExp e2
  if val2 == 0 then lift $ lift $ throwE "Division by zero"
    else return $ Int $ val1 `div` val2
  
transExp (EPostInc e) = transExp $ EAssign e AssignAdd (EConst $ EInt 1)
transExp (EPostDec e) = transExp $ EAssign e AssignSub (EConst $ EInt 1)


transExp (ELthen e1 e2) = evalBinOpBool e1 e2 (<)
transExp (EGrthen e1 e2) = evalBinOpBool e1 e2 (>)
transExp (ELe e1 e2) = evalBinOpBool e1 e2 (<=)
transExp (EGe e1 e2) = evalBinOpBool e1 e2 (>=)

transExp (ENegative e) = do
  (Int val) <- transExp e
  return $ Int $ (-1)*val

transExp (EFunk (EVar f)) = transExp (EFunkPar (EVar f) []) 
  
transExp (EFunkPar (EVar f) exps) = do
  arguments <- mapM transExp exps
  (Fun fun) <- getFun f
  fun arguments

transExp input@(EArray a exp) = do
  (Int i) <- transExp $ exp
  (Array n array) <- transExp a
  if i >= 0 && i < n then if member i array then getLocVal $ array ! i
                          else lift $ lift $ throwE $ "The element in the array was not initialized: " ++ printTree input
    else lift $ lift $ throwE $ "Index out of bounds, index: " ++ (show i) ++ ", size: " ++ (show n) 

transExp input@(EMap m exp) = do
  key <- transExp exp
  (Mapp map) <- transExp m
  if member key map then getLocVal $ map ! key
    else lift $ lift $ throwE $ "There is no such element in the map: " ++ printTree input

transExp input@(ESelect s field) = do
  (Structt struct) <- transExp s
  if member field struct then  getLocVal $ struct ! field
    else lift $ lift $ throwE $ "The field was not initialized: " ++ printTree input

evalBinOpInt :: Exp -> Exp -> (Int -> Int -> Int) -> Interpreter Val
evalBinOpInt e1 e2 op = do
  (Int val1) <- transExp e1
  (Int val2) <- transExp e2
  return $ Int $ op val1 val2

evalBinOpBool :: Exp -> Exp -> (Int -> Int -> Bool) -> Interpreter Val
evalBinOpBool e1 e2 op = do
  (Int val1) <- transExp e1
  (Int val2) <- transExp e2
  return $ Bool $ op val1 val2


deepCopy :: Val -> Interpreter Val
deepCopy (Int v) = return $ Int v
deepCopy (Bool v) = return $ Bool v
deepCopy (Array n arr) = do
  newArr <- mapM copyAux arr
  return $ Array n newArr
deepCopy (Mapp map) = do
  newMap <- mapM copyAux map
  return $ Mapp newMap
deepCopy (Structt struct) = do
  newStruct <- mapM copyAux struct
  return $ Structt newStruct


copyAux :: Loc -> Interpreter Loc
copyAux loc = do
  newLoc <- alloc
  store <- get
  val <- deepCopy $ store ! loc
  modify (insert newLoc val)
  return newLoc

toLoc :: Exp -> Interpreter Loc

toLoc (EVar var) = getVarLoc var

toLoc (EArray v exp) = do
  (Int i) <- transExp exp
  arrLoc <- toLoc v
  val <- getLocVal arrLoc
  let (n, arr) = case val of
        Null -> (0, Data.Map.empty)
        (Array n' arr') -> (n', arr')
  if i < 0 || i >= n then lift $ lift $ throwE $ "Index out of bounds, index: " ++ (show i) ++ ", size: " ++ (show n)
    else do
    if member i arr then do
      let loc = arr ! i
      return loc
      else do
      loc <- alloc
      let newArr = Array n $ insert i loc arr
      modify (\store -> insert arrLoc newArr store)
      return loc

toLoc (EMap v exp) = do
  key <- transExp exp
  mapLoc <- toLoc v
  val <- getLocVal mapLoc
  let map = case val of
        Null -> Data.Map.empty
        (Mapp map') -> map'
  if member key map then do
     let loc = map ! key
     return loc
    else do
     loc <- alloc
     let newMap = Mapp $ insert key loc map
     modify (\store -> insert mapLoc newMap store)
     return loc


toLoc (ESelect v field) = do
  structLoc <- toLoc v
  val <- getLocVal structLoc
  let struct = case val of
        Null -> Data.Map.empty 
        (Structt s) -> s
  if member field struct then do
    return $ struct ! field
    else do
    loc <- alloc
    let newStruct = Structt $ insert field loc struct
    modify (insert structLoc newStruct)
    return loc

assign :: Exp -> Val -> Interpreter ()

assign exp val = do
  loc <- toLoc exp
  valCopy <- deepCopy val
  modify (insert loc valCopy) 

-- Declaration evaluations
transDec :: [Dec] -> Interpreter Env

transDec [] = ask
transDec ((Declaration (DVariable t v)):ds) = do
  loc <- alloc
  modify (\store -> insert loc (initialValue t) store)
  local (setLoc v loc) $ transDec ds

initialValue :: TypeSpecifier -> Val
initialValue TInt = Int 0
initialValue TBool = Bool False
initialValue (TArray _) = Array 0 Data.Map.empty
initialValue (TMap _ _) = Mapp Data.Map.empty
initialValue (TStruct _) = Structt Data.Map.empty

transExternalDeclaration :: ExternalDeclaration -> Interpreter Env
transExternalDeclaration x = case x of
  Afunc functiondef  -> transFuncDef functiondef
  Global dec  -> transDec [dec]
  StructDec structspec  -> transStructDec structspec

transFuncDef :: FunctionDef -> Interpreter Env
transFuncDef (FuncParams (DVariable _ funName) params (FuncBodyOne ds fs stmts es)) = do
  env <- ask
  let fun arguments = do
        env' <- local (const env) $ transArguments params arguments -- params
        env'' <- local (const env') $ transDec ds -- declarations
        let env'''  = setFun funName (Fun fun) env'' -- recursion
        env'''' <- local (const env''') $ transFuncDefs fs -- inner functions
        local (const env'''') $ transStmts stmts
        case es of
          SExprOne -> return $ Int 0 -- procedure, returning whatever
          SExprTwo e -> local (const env'''') $ transExp e
  return $ setFun funName (Fun fun) env

transFuncDefs :: [FunctionDef] -> Interpreter Env
transFuncDefs [] = ask
transFuncDefs (f:fs) = do
  newEnv1 <- transFuncDef f
  local (const newEnv1) $ transFuncDefs fs

transArguments :: [Declarator] -> [Val] -> Interpreter Env
transArguments [] [] = ask
transArguments ((DVariable _ var):ds) (v:vs) = do
  loc <- alloc
  modify (\store -> insert loc v store)
  newEnv <- local (setLoc var loc) $ transArguments ds vs
  return newEnv

transStructDec :: StructSpec -> Interpreter Env
-- Struct declarations are only necessary for static analysis
transStructDec _ = ask

transExternalDeclarations :: [ExternalDeclaration] -> Interpreter Env
transExternalDeclarations [] = ask
transExternalDeclarations (d:ds) = do
  newEnv1 <- transExternalDeclaration d
  newEnv2 <- local (const newEnv1) $ transExternalDeclarations ds
  return newEnv2

-- Program execution
transProgram :: Program -> Interpreter ()
transProgram (Progr ds) = do
  env <- transExternalDeclarations ds
  (Fun main) <- local (const env) $ getFun (Ident "main")
  local (const env) $ main []
  return ()
