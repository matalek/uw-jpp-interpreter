{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintMatal where

-- pretty-printer generated by the BNF converter

import AbsMatal
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))



instance Print Program where
  prt i e = case e of
    Progr externaldeclarations -> prPrec i 0 (concatD [prt 0 externaldeclarations])

instance Print ExternalDeclaration where
  prt i e = case e of
    Afunc functiondef -> prPrec i 0 (concatD [prt 0 functiondef])
    Global dec -> prPrec i 0 (concatD [prt 0 dec])
    StructDec structspec -> prPrec i 0 (concatD [prt 0 structspec])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Declarator where
  prt i e = case e of
    DVariable typespecifier id -> prPrec i 0 (concatD [prt 0 typespecifier, prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Dec where
  prt i e = case e of
    Declaration declarator -> prPrec i 0 (concatD [prt 0 declarator, doc (showString ";")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print TypeSpecifier where
  prt i e = case e of
    TVoid -> prPrec i 0 (concatD [doc (showString "void")])
    TInt -> prPrec i 0 (concatD [doc (showString "int")])
    TBool -> prPrec i 0 (concatD [doc (showString "bool")])
    TStruct id -> prPrec i 0 (concatD [doc (showString "struct"), prt 0 id])
    TArray typespecifier -> prPrec i 0 (concatD [prt 0 typespecifier, doc (showString "[]")])
    TMap typespecifier1 typespecifier2 -> prPrec i 0 (concatD [prt 0 typespecifier1, doc (showString "<<"), prt 0 typespecifier2, doc (showString ">>")])

instance Print StructSpec where
  prt i e = case e of
    Struct id decs -> prPrec i 0 (concatD [doc (showString "struct"), prt 0 id, doc (showString "{"), prt 0 decs, doc (showString "}")])

instance Print FunctionDef where
  prt i e = case e of
    FuncParams declarator declarators functionbody -> prPrec i 0 (concatD [doc (showString "function"), prt 0 declarator, doc (showString "("), prt 0 declarators, doc (showString ")"), prt 0 functionbody])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print FunctionBody where
  prt i e = case e of
    FuncBodyOne decs functiondefs stmts expressionstmt -> prPrec i 0 (concatD [doc (showString "{"), prt 0 decs, prt 0 functiondefs, prt 0 stmts, doc (showString "return"), prt 0 expressionstmt, doc (showString "}")])

instance Print Stmt where
  prt i e = case e of
    SComp compoundstmt -> prPrec i 0 (concatD [prt 0 compoundstmt])
    SExpr expressionstmt -> prPrec i 0 (concatD [prt 0 expressionstmt])
    SSel selectionstmt -> prPrec i 0 (concatD [prt 0 selectionstmt])
    SIter iterstmt -> prPrec i 0 (concatD [prt 0 iterstmt])
    SPrint printstmt -> prPrec i 0 (concatD [prt 0 printstmt])
    SInit initstmt -> prPrec i 0 (concatD [prt 0 initstmt])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print CompoundStmt where
  prt i e = case e of
    SCompOne decs stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 decs, prt 0 stmts, doc (showString "}")])

instance Print ExpressionStmt where
  prt i e = case e of
    SExprOne -> prPrec i 0 (concatD [doc (showString ";")])
    SExprTwo exp -> prPrec i 0 (concatD [prt 0 exp, doc (showString ";")])

instance Print SelectionStmt where
  prt i e = case e of
    SSelOne exp compoundstmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 compoundstmt])
    SSelTwo exp compoundstmt1 compoundstmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 compoundstmt1, doc (showString "else"), prt 0 compoundstmt2])

instance Print IterStmt where
  prt i e = case e of
    SIterOne exp stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stmt])
    SIterTwo expressionstmt1 expressionstmt2 stmt -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 expressionstmt1, prt 0 expressionstmt2, doc (showString ")"), prt 0 stmt])
    SIterThree expressionstmt1 expressionstmt2 exp stmt -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 expressionstmt1, prt 0 expressionstmt2, prt 0 exp, doc (showString ")"), prt 0 stmt])

instance Print PrintStmt where
  prt i e = case e of
    SPrintOne exp -> prPrec i 0 (concatD [doc (showString "print"), prt 0 exp, doc (showString ";")])

instance Print InitStmt where
  prt i e = case e of
    SInitOne exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "init"), prt 0 exp2, doc (showString ";")])

instance Print Exp where
  prt i e = case e of
    EComma exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString ","), prt 1 exp2])
    EAssign exp1 assignmentop exp2 -> prPrec i 1 (concatD [prt 4 exp1, prt 0 assignmentop, prt 1 exp2])
    EEq exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "=="), prt 3 exp2])
    ENeq exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "!="), prt 3 exp2])
    ELthen exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "<"), prt 4 exp2])
    EGrthen exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString ">"), prt 4 exp2])
    ELe exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "<="), prt 4 exp2])
    EGe exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString ">="), prt 4 exp2])
    EPlus exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "+"), prt 5 exp2])
    EMinus exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "-"), prt 5 exp2])
    ETimes exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "*"), prt 6 exp2])
    EDiv exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "/"), prt 6 exp2])
    ENegative exp -> prPrec i 6 (concatD [doc (showString "-"), prt 6 exp])
    ESelect exp id -> prPrec i 7 (concatD [prt 7 exp, doc (showString "."), prt 0 id])
    EArray exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "["), prt 0 exp2, doc (showString "]")])
    EFunk exp -> prPrec i 7 (concatD [prt 7 exp, doc (showString "("), doc (showString ")")])
    EFunkPar exp exps -> prPrec i 7 (concatD [prt 7 exp, doc (showString "("), prt 1 exps, doc (showString ")")])
    EMap exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "<<"), prt 0 exp2, doc (showString ">>")])
    EPostInc exp -> prPrec i 8 (concatD [prt 8 exp, doc (showString "++")])
    EPostDec exp -> prPrec i 8 (concatD [prt 8 exp, doc (showString "--")])
    EVar id -> prPrec i 9 (concatD [prt 0 id])
    EConst constant -> prPrec i 9 (concatD [prt 0 constant])
  prtList 1 [x] = (concatD [prt 1 x])
  prtList 1 (x:xs) = (concatD [prt 1 x, doc (showString ","), prt 1 xs])
instance Print Constant where
  prt i e = case e of
    EInt n -> prPrec i 0 (concatD [prt 0 n])
    ETrue -> prPrec i 0 (concatD [doc (showString "true")])
    EFalse -> prPrec i 0 (concatD [doc (showString "false")])

instance Print AssignmentOp where
  prt i e = case e of
    Assign -> prPrec i 0 (concatD [doc (showString "=")])
    AssignMul -> prPrec i 0 (concatD [doc (showString "*=")])
    AssignDiv -> prPrec i 0 (concatD [doc (showString "/=")])
    AssignAdd -> prPrec i 0 (concatD [doc (showString "+=")])
    AssignSub -> prPrec i 0 (concatD [doc (showString "-=")])


