{-# OPTIONS -fno-warn-incomplete-patterns #-}
module Printmatal where

-- pretty-printer generated by the BNF converter

import Absmatal
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
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

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
  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])



instance Print Program where
  prt i e = case e of
   Progr external_declarations -> prPrec i 0 (concatD [prt 0 external_declarations])


instance Print External_declaration where
  prt i e = case e of
   Afunc function_def -> prPrec i 0 (concatD [prt 0 function_def])
   Global dec -> prPrec i 0 (concatD [prt 0 dec])
   StructDec struct_spec -> prPrec i 0 (concatD [prt 0 struct_spec])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Declarator where
  prt i e = case e of
   Dvariable type_specifier id -> prPrec i 0 (concatD [prt 0 type_specifier , prt 0 id])


instance Print Dec where
  prt i e = case e of
   Declaration declarator -> prPrec i 0 (concatD [prt 0 declarator , doc (showString ";")])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Type_specifier where
  prt i e = case e of
   Tvoid  -> prPrec i 0 (concatD [doc (showString "void")])
   Tint  -> prPrec i 0 (concatD [doc (showString "int")])
   Tbool  -> prPrec i 0 (concatD [doc (showString "bool")])
   Tstruct id -> prPrec i 0 (concatD [prt 0 id])
   Tarray type_specifier -> prPrec i 0 (concatD [prt 0 type_specifier , doc (showString "[]")])
   Tmap type_specifier0 type_specifier -> prPrec i 0 (concatD [prt 0 type_specifier0 , doc (showString "<<") , prt 0 type_specifier , doc (showString ">>")])


instance Print Struct_spec where
  prt i e = case e of
   Struct id decs -> prPrec i 0 (concatD [doc (showString "struct") , prt 0 id , doc (showString "{") , prt 0 decs , doc (showString "}")])


instance Print Function_def where
  prt i e = case e of
   FuncNoParams declarator compound_stm -> prPrec i 0 (concatD [prt 0 declarator , doc (showString "(") , doc (showString ")") , prt 0 compound_stm])
   FuncParams declarator parameter_declarations compound_stm -> prPrec i 0 (concatD [prt 0 declarator , doc (showString "(") , prt 0 parameter_declarations , doc (showString ")") , prt 0 compound_stm])


instance Print Parameter_declarations where
  prt i e = case e of
   ParamDec declarator -> prPrec i 0 (concatD [prt 0 declarator])
   MoreParamDec parameter_declarations declarator -> prPrec i 0 (concatD [prt 0 parameter_declarations , doc (showString ",") , prt 0 declarator])


instance Print Stm where
  prt i e = case e of
   CompS compound_stm -> prPrec i 0 (concatD [prt 0 compound_stm])
   ExprS expression_stm -> prPrec i 0 (concatD [prt 0 expression_stm])
   SelS selection_stm -> prPrec i 0 (concatD [prt 0 selection_stm])
   IterS iter_stm -> prPrec i 0 (concatD [prt 0 iter_stm])
   JumpS jump_stm -> prPrec i 0 (concatD [prt 0 jump_stm])
   PrintS print_stm -> prPrec i 0 (concatD [prt 0 print_stm])
   InitS init_stm -> prPrec i 0 (concatD [prt 0 init_stm])

  prtList es = case es of
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Compound_stm where
  prt i e = case e of
   ScompOne  -> prPrec i 0 (concatD [doc (showString "{") , doc (showString "}")])
   ScompTwo stms -> prPrec i 0 (concatD [doc (showString "{") , prt 0 stms , doc (showString "}")])
   ScompThree decs stms -> prPrec i 0 (concatD [doc (showString "{") , prt 0 decs , prt 0 stms , doc (showString "}")])


instance Print Expression_stm where
  prt i e = case e of
   SexprOne  -> prPrec i 0 (concatD [doc (showString ";")])
   SexprTwo exp -> prPrec i 0 (concatD [prt 0 exp , doc (showString ";")])


instance Print Selection_stm where
  prt i e = case e of
   SselOne exp stm -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 stm])
   SselTwo exp stm0 stm -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 stm0 , doc (showString "else") , prt 0 stm])


instance Print Iter_stm where
  prt i e = case e of
   SiterOne exp stm -> prPrec i 0 (concatD [doc (showString "while") , doc (showString "(") , prt 0 exp , doc (showString ")") , prt 0 stm])
   SiterThree expression_stm0 expression_stm stm -> prPrec i 0 (concatD [doc (showString "for") , doc (showString "(") , prt 0 expression_stm0 , prt 0 expression_stm , doc (showString ")") , prt 0 stm])
   SiterFour expression_stm0 expression_stm exp stm -> prPrec i 0 (concatD [doc (showString "for") , doc (showString "(") , prt 0 expression_stm0 , prt 0 expression_stm , prt 0 exp , doc (showString ")") , prt 0 stm])


instance Print Jump_stm where
  prt i e = case e of
   SjumpFive exp -> prPrec i 0 (concatD [doc (showString "return") , prt 0 exp , doc (showString ";")])


instance Print Print_stm where
  prt i e = case e of
   Sprint exp -> prPrec i 0 (concatD [doc (showString "print") , prt 0 exp , doc (showString ";")])


instance Print Init_stm where
  prt i e = case e of
   Sinit id exp -> prPrec i 0 (concatD [doc (showString "init") , prt 0 id , doc (showString "[") , prt 0 exp , doc (showString "]") , doc (showString ";")])


instance Print Exp where
  prt i e = case e of
   Ecomma exp0 exp -> prPrec i 0 (concatD [prt 0 exp0 , doc (showString ",") , prt 1 exp])
   Eassign exp0 assignment_op exp -> prPrec i 1 (concatD [prt 4 exp0 , prt 0 assignment_op , prt 1 exp])
   Eeq exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "==") , prt 3 exp])
   Eneq exp0 exp -> prPrec i 2 (concatD [prt 2 exp0 , doc (showString "!=") , prt 3 exp])
   Elthen exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString "<") , prt 4 exp])
   Egrthen exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString ">") , prt 4 exp])
   Ele exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString "<=") , prt 4 exp])
   Ege exp0 exp -> prPrec i 3 (concatD [prt 3 exp0 , doc (showString ">=") , prt 4 exp])
   Eplus exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString "+") , prt 3 exp])
   Eminus exp0 exp -> prPrec i 4 (concatD [prt 4 exp0 , doc (showString "-") , prt 3 exp])
   Etimes exp0 exp -> prPrec i 5 (concatD [prt 5 exp0 , doc (showString "*") , prt 4 exp])
   Ediv exp0 exp -> prPrec i 5 (concatD [prt 5 exp0 , doc (showString "/") , prt 4 exp])
   Eselect exp id -> prPrec i 6 (concatD [prt 6 exp , doc (showString ".") , prt 0 id])
   Earray exp0 exp -> prPrec i 6 (concatD [prt 6 exp0 , doc (showString "[") , prt 0 exp , doc (showString "]")])
   Efunk exp -> prPrec i 6 (concatD [prt 6 exp , doc (showString "(") , doc (showString ")")])
   Efunkpar exp exps -> prPrec i 6 (concatD [prt 6 exp , doc (showString "(") , prt 1 exps , doc (showString ")")])
   Emap exp0 exp -> prPrec i 6 (concatD [prt 6 exp0 , doc (showString "<<") , prt 0 exp , doc (showString ">>")])
   Epostinc exp -> prPrec i 7 (concatD [prt 7 exp , doc (showString "++")])
   Epostdec exp -> prPrec i 7 (concatD [prt 7 exp , doc (showString "--")])
   Evar id -> prPrec i 8 (concatD [prt 0 id])
   Econst constant -> prPrec i 8 (concatD [prt 0 constant])

  prtList es = case es of
   [x] -> (concatD [prt 1 x])
   x:xs -> (concatD [prt 1 x , doc (showString ",") , prt 1 xs])

instance Print Constant where
  prt i e = case e of
   Eint n -> prPrec i 0 (concatD [prt 0 n])
   Etrue  -> prPrec i 0 (concatD [doc (showString "true")])
   Efalse  -> prPrec i 0 (concatD [doc (showString "false")])


instance Print Assignment_op where
  prt i e = case e of
   Assign  -> prPrec i 0 (concatD [doc (showString "=")])
   AssignMul  -> prPrec i 0 (concatD [doc (showString "*=")])
   AssignDiv  -> prPrec i 0 (concatD [doc (showString "/=")])
   AssignAdd  -> prPrec i 0 (concatD [doc (showString "+=")])
   AssignSub  -> prPrec i 0 (concatD [doc (showString "-=")])


