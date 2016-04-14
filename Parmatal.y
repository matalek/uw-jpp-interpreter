-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parmatal where
import Absmatal
import Lexmatal
import ErrM

}

%name pProgram Program
%name pStm Stm
%name pExp Exp

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '!=' { PT _ (TS _ 1) }
 '(' { PT _ (TS _ 2) }
 ')' { PT _ (TS _ 3) }
 '*' { PT _ (TS _ 4) }
 '*=' { PT _ (TS _ 5) }
 '+' { PT _ (TS _ 6) }
 '++' { PT _ (TS _ 7) }
 '+=' { PT _ (TS _ 8) }
 ',' { PT _ (TS _ 9) }
 '-' { PT _ (TS _ 10) }
 '--' { PT _ (TS _ 11) }
 '-=' { PT _ (TS _ 12) }
 '.' { PT _ (TS _ 13) }
 '/' { PT _ (TS _ 14) }
 '/=' { PT _ (TS _ 15) }
 ';' { PT _ (TS _ 16) }
 '<' { PT _ (TS _ 17) }
 '<<' { PT _ (TS _ 18) }
 '<=' { PT _ (TS _ 19) }
 '=' { PT _ (TS _ 20) }
 '==' { PT _ (TS _ 21) }
 '>' { PT _ (TS _ 22) }
 '>=' { PT _ (TS _ 23) }
 '>>' { PT _ (TS _ 24) }
 '[' { PT _ (TS _ 25) }
 '[]' { PT _ (TS _ 26) }
 ']' { PT _ (TS _ 27) }
 'bool' { PT _ (TS _ 28) }
 'else' { PT _ (TS _ 29) }
 'false' { PT _ (TS _ 30) }
 'for' { PT _ (TS _ 31) }
 'if' { PT _ (TS _ 32) }
 'init' { PT _ (TS _ 33) }
 'int' { PT _ (TS _ 34) }
 'print' { PT _ (TS _ 35) }
 'return' { PT _ (TS _ 36) }
 'struct' { PT _ (TS _ 37) }
 'true' { PT _ (TS _ 38) }
 'void' { PT _ (TS _ 39) }
 'while' { PT _ (TS _ 40) }
 '{' { PT _ (TS _ 41) }
 '}' { PT _ (TS _ 42) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }

Program :: { Program }
Program : ListExternal_declaration { Progr $1 } 


ListExternal_declaration :: { [External_declaration] }
ListExternal_declaration : External_declaration { (:[]) $1 } 
  | External_declaration ListExternal_declaration { (:) $1 $2 }


External_declaration :: { External_declaration }
External_declaration : Function_def { Afunc $1 } 
  | Dec { Global $1 }
  | Struct_spec { StructDec $1 }


Declarator :: { Declarator }
Declarator : Type_specifier Ident { Dvariable $1 $2 } 


Dec :: { Dec }
Dec : Declarator ';' { Declaration $1 } 


ListDec :: { [Dec] }
ListDec : Dec { (:[]) $1 } 
  | Dec ListDec { (:) $1 $2 }


Type_specifier :: { Type_specifier }
Type_specifier : 'void' { Tvoid } 
  | 'int' { Tint }
  | 'bool' { Tbool }
  | Ident { Tstruct $1 }
  | Type_specifier '[]' { Tarray $1 }
  | Type_specifier '<<' Type_specifier '>>' { Tmap $1 $3 }


Struct_spec :: { Struct_spec }
Struct_spec : 'struct' Ident '{' ListDec '}' { Struct $2 $4 } 


ListIdent :: { [Ident] }
ListIdent : Ident { (:[]) $1 } 
  | Ident ',' ListIdent { (:) $1 $3 }


Function_def :: { Function_def }
Function_def : Declarator '(' ')' Compound_stm { FuncNoParams $1 $4 } 
  | Declarator '(' Parameter_declarations ')' Compound_stm { FuncParams $1 $3 $5 }


Parameter_declarations :: { Parameter_declarations }
Parameter_declarations : Declarator { ParamDec $1 } 
  | Parameter_declarations ',' Declarator { MoreParamDec $1 $3 }


Stm :: { Stm }
Stm : Compound_stm { CompS $1 } 
  | Expression_stm { ExprS $1 }
  | Selection_stm { SelS $1 }
  | Iter_stm { IterS $1 }
  | Jump_stm { JumpS $1 }
  | Print_stm { PrintS $1 }
  | Init_stm { InitS $1 }


Compound_stm :: { Compound_stm }
Compound_stm : '{' '}' { ScompOne } 
  | '{' ListStm '}' { ScompTwo $2 }
  | '{' ListDec ListStm '}' { ScompThree $2 $3 }


Expression_stm :: { Expression_stm }
Expression_stm : ';' { SexprOne } 
  | Exp ';' { SexprTwo $1 }


Selection_stm :: { Selection_stm }
Selection_stm : 'if' '(' Exp ')' Stm { SselOne $3 $5 } 
  | 'if' '(' Exp ')' Stm 'else' Stm { SselTwo $3 $5 $7 }


Iter_stm :: { Iter_stm }
Iter_stm : 'while' '(' Exp ')' Stm { SiterOne $3 $5 } 
  | 'for' '(' Expression_stm Expression_stm ')' Stm { SiterThree $3 $4 $6 }
  | 'for' '(' Expression_stm Expression_stm Exp ')' Stm { SiterFour $3 $4 $5 $7 }


Jump_stm :: { Jump_stm }
Jump_stm : 'return' Exp ';' { SjumpFive $2 } 


Print_stm :: { Print_stm }
Print_stm : 'print' Exp ';' { Sprint $2 } 


Init_stm :: { Init_stm }
Init_stm : 'init' Ident '[' Exp ']' ';' { Sinit $2 $4 } 


ListStm :: { [Stm] }
ListStm : Stm { (:[]) $1 } 
  | Stm ListStm { (:) $1 $2 }


Exp :: { Exp }
Exp : Exp ',' Exp1 { Ecomma $1 $3 } 
  | Exp1 { $1 }


Exp1 :: { Exp }
Exp1 : Exp4 Assignment_op Exp1 { Eassign $1 $2 $3 } 
  | Exp2 { $1 }


Exp2 :: { Exp }
Exp2 : Exp2 '==' Exp3 { Eeq $1 $3 } 
  | Exp2 '!=' Exp3 { Eneq $1 $3 }
  | Exp3 { $1 }


Exp3 :: { Exp }
Exp3 : Exp3 '<' Exp4 { Elthen $1 $3 } 
  | Exp3 '>' Exp4 { Egrthen $1 $3 }
  | Exp3 '<=' Exp4 { Ele $1 $3 }
  | Exp3 '>=' Exp4 { Ege $1 $3 }
  | Exp4 { $1 }


Exp4 :: { Exp }
Exp4 : Exp4 '+' Exp3 { Eplus $1 $3 } 
  | Exp4 '-' Exp3 { Eminus $1 $3 }
  | Exp5 { $1 }


Exp5 :: { Exp }
Exp5 : Exp5 '*' Exp4 { Etimes $1 $3 } 
  | Exp5 '/' Exp4 { Ediv $1 $3 }
  | Exp6 { $1 }


Exp6 :: { Exp }
Exp6 : Exp6 '.' Ident { Eselect $1 $3 } 
  | Exp6 '[' Exp ']' { Earray $1 $3 }
  | Exp6 '(' ')' { Efunk $1 }
  | Exp6 '(' ListExp1 ')' { Efunkpar $1 $3 }
  | Exp6 '<<' Exp '>>' { Emap $1 $3 }
  | Exp7 { $1 }


Exp7 :: { Exp }
Exp7 : Exp7 '++' { Epostinc $1 } 
  | Exp7 '--' { Epostdec $1 }
  | Exp8 { $1 }


Exp8 :: { Exp }
Exp8 : Ident { Evar $1 } 
  | Constant { Econst $1 }
  | '(' Exp ')' { $2 }


Constant :: { Constant }
Constant : Integer { Eint $1 } 
  | 'true' { Etrue }
  | 'false' { Efalse }


ListExp1 :: { [Exp] }
ListExp1 : Exp1 { (:[]) $1 } 
  | Exp1 ',' ListExp1 { (:) $1 $3 }


Assignment_op :: { Assignment_op }
Assignment_op : '=' { Assign } 
  | '*=' { AssignMul }
  | '/=' { AssignDiv }
  | '+=' { AssignAdd }
  | '-=' { AssignSub }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

