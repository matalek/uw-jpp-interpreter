module Interpreter where

import AbsMatal
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Map
import ErrM

type Var = Ident
type FName = Ident
type Loc = Int
data Val = Int Int | Bool Bool
newtype Fun = Fun ([Val] -> Interpreter Val)

type Store = Map Loc Val
type Env = (Map Var Loc, Map FName Fun)

type Interpreter a = StateT Store (ReaderT Env IO) a

failure :: Show a => a -> Interpreter ()
failure x = error $ "Undefined case: " ++ show x

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

setVal :: Var -> Val -> Interpreter ()
setVal var val = do
  loc <- getVarLoc var
  modify $ insert loc val

showVal :: Val -> String
showVal (Int i) = show i
showVal (Bool b)
  | b = "true"
  | otherwise = "false"

alloc :: Interpreter Loc
alloc = do
  store <- get
  let loc = if size store /= 0 then (fst $ findMax store) else 0 + 1
  put $ insert loc (Int 0) store
  return loc

skip :: Stmt
skip = SComp (SCompOne [] [])

-- Right now only for statements
interpret :: Program -> IO ()
interpret p = do
  runReaderT (execStateT (transProgram p) empty) (empty, empty)
  return ()

transStmts :: [Stmt] -> Interpreter ()
transStmts [] = return ()
transStmts (s:ss) = do
  transStmt s
  transStmts ss

-- Statement execution
transStmt :: Stmt -> Interpreter ()
transStmt x = case x of
  SComp compoundStmt  -> transCompoundStmt compoundStmt
  SExpr expressionStmt  -> transExpressionStmt expressionStmt
  SSel selectionStmt  -> transSelectionStmt selectionStmt
  SIter iterStmt  -> transIterStmt iterStmt
  SPrint printStmt  -> transPrintStmt printStmt
  SInit initStmt  -> failure x


-- Expression statements
transExpressionStmt :: ExpressionStmt -> Interpreter ()
transExpressionStmt SExprOne = return ()
transExpressionStmt (SExprTwo e) = do
  _ <- transExp e
  return ()

-- Selection statements
transSelectionStmt :: SelectionStmt -> Interpreter ()
transSelectionStmt (SSelOne e s) =
  transSelectionStmt (SSelTwo e s skip) 
transSelectionStmt (SSelTwo e s1 s2) = do
  val <- transExp e
  case val of
    (Bool True) -> transStmt s1
    _ -> transStmt s2

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
  lift $ lift $ putStrLn $ showVal val

-- Compound statements
transCompoundStmt :: CompoundStmt -> Interpreter ()
transCompoundStmt (SCompOne ds ss) = do
  newEnv <- transDec ds
  local (\_ -> newEnv) $ transStmts ss

-- Expression evaluation
transExp :: Exp -> Interpreter Val

transExp (EConst c) = case c of
  (EInt i) -> return $ Int (fromInteger i)
  ETrue -> return $ Bool True
  EFalse -> return $ Bool False

transExp (EVar v) = do getVarVal v

transExp (EAssign e1@(EVar v) op e2) = do
  newVal <-
    case op of
      Assign -> transExp e2
      AssignAdd -> transExp (EPlus e1 e2)
      AssignSub -> transExp (EMinus e1 e2)
      AssignMul -> transExp (ETimes e1 e2)
      AssignDiv -> transExp (EDiv e1 e2)
  setVal v newVal
  return newVal

transExp (EPlus e1 e2) = evalBinOpInt e1 e2 (+)
transExp (EMinus e1 e2) = evalBinOpInt e1 e2 (-)
transExp (ETimes e1 e2) = evalBinOpInt e1 e2 (*)
transExp (EDiv e1 e2) = evalBinOpInt e1 e2 div

transExp (ELthen e1 e2) = evalBinOpBool e1 e2 (<)
transExp (EGrthen e1 e2) = evalBinOpBool e1 e2 (>)
transExp (ELe e1 e2) = evalBinOpBool e1 e2 (<=)
transExp (EGe e1 e2) = evalBinOpBool e1 e2 (>=)

transExp (EFunk (EVar f)) = do
  (Fun fun) <- getFun f
  fun []

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

-- Declaration evaluations
transDec :: [Dec] -> Interpreter Env

transDec [] = ask
transDec ((Declaration (DVariable TInt v)):ds) = do
  loc <- alloc
  modify (\store -> insert loc (Int 0) store)
  newEnv <- local (setLoc v loc) $ transDec ds
  return newEnv


transExternalDeclaration :: ExternalDeclaration -> Interpreter Env
transExternalDeclaration x = case x of
  Afunc functiondef  -> transFuncDef functiondef
  Global dec  -> transDec [dec]
  -- StructDec structspec  -> failure x

transFuncDef :: FunctionDef -> Interpreter Env
transFuncDef (FuncNoParams (DVariable _ funName) (FuncBodyOne ds stmts es)) = do
  env <- ask
  newEnv <- transDec ds
  let fun _ = do
        local (\_ -> newEnv) $ transStmts stmts
        case es of
          SExprOne -> return $ Int 0 -- procedure, returning whatever
          SExprTwo e -> local (\_ -> newEnv) $ transExp e
  return $ setFun funName (Fun fun) newEnv

transExternalDeclarations :: [ExternalDeclaration] -> Interpreter Env
transExternalDeclarations [] = ask
transExternalDeclarations (d:ds) = do
  newEnv1 <- transExternalDeclaration d
  newEnv2 <- local (\_ -> newEnv1) $ transExternalDeclarations ds
  return newEnv2

-- Program execution
transProgram :: Program -> Interpreter ()
transProgram (Progr ds) = do
  env <- transExternalDeclarations ds
  (Fun main) <- local (\_ -> env) $ getFun (Ident "main")
  local (\_ -> env) $ main []
  return ()
