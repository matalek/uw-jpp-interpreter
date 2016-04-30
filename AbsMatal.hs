

module AbsMatal where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program = Progr [ExternalDeclaration]
  deriving (Eq, Ord, Show, Read)

data ExternalDeclaration
    = Afunc FunctionDef | Global Dec | StructDec StructSpec
  deriving (Eq, Ord, Show, Read)

data Declarator = DVariable TypeSpecifier Ident
  deriving (Eq, Ord, Show, Read)

data Dec = Declaration Declarator
  deriving (Eq, Ord, Show, Read)

data TypeSpecifier
    = TVoid
    | TInt
    | TBool
    | TStruct Ident
    | TArray TypeSpecifier
    | TMap TypeSpecifier TypeSpecifier
  deriving (Eq, Ord, Show, Read)

data StructSpec = Struct Ident [Dec]
  deriving (Eq, Ord, Show, Read)

data FunctionDef = FuncParams Declarator [Declarator] FunctionBody
  deriving (Eq, Ord, Show, Read)

data FunctionBody
    = FuncBodyOne [Dec] [FunctionDef] [Stmt] ExpressionStmt
  deriving (Eq, Ord, Show, Read)

data Stmt
    = SComp CompoundStmt
    | SExpr ExpressionStmt
    | SSel SelectionStmt
    | SIter IterStmt
    | SPrint PrintStmt
    | SInit InitStmt
  deriving (Eq, Ord, Show, Read)

data CompoundStmt = SCompOne [Dec] [Stmt]
  deriving (Eq, Ord, Show, Read)

data ExpressionStmt = SExprOne | SExprTwo Exp
  deriving (Eq, Ord, Show, Read)

data SelectionStmt
    = SSelOne Exp CompoundStmt | SSelTwo Exp CompoundStmt CompoundStmt
  deriving (Eq, Ord, Show, Read)

data IterStmt
    = SIterOne Exp Stmt
    | SIterTwo ExpressionStmt ExpressionStmt Stmt
    | SIterThree ExpressionStmt ExpressionStmt Exp Stmt
  deriving (Eq, Ord, Show, Read)

data PrintStmt = SPrintOne Exp
  deriving (Eq, Ord, Show, Read)

data InitStmt = SInitOne Exp Exp
  deriving (Eq, Ord, Show, Read)

data Exp
    = EComma Exp Exp
    | EAssign Exp AssignmentOp Exp
    | EEq Exp Exp
    | ENeq Exp Exp
    | ELthen Exp Exp
    | EGrthen Exp Exp
    | ELe Exp Exp
    | EGe Exp Exp
    | EPlus Exp Exp
    | EMinus Exp Exp
    | ETimes Exp Exp
    | EDiv Exp Exp
    | ENegative Exp
    | ESelect Exp Ident
    | EArray Exp Exp
    | EFunk Exp
    | EFunkPar Exp [Exp]
    | EMap Exp Exp
    | EPostInc Exp
    | EPostDec Exp
    | EVar Ident
    | EConst Constant
  deriving (Eq, Ord, Show, Read)

data Constant = EInt Integer | ETrue | EFalse
  deriving (Eq, Ord, Show, Read)

data AssignmentOp
    = Assign | AssignMul | AssignDiv | AssignAdd | AssignSub
  deriving (Eq, Ord, Show, Read)

