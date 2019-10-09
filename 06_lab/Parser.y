{
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Parser where
import GHC.Generics
import Lexer
import Control.DeepSeq
import Data.Char
}

%name parser Expr
%name parserContext Context
%tokentype { Token }
%error { parseError }

%token
    var  {TVar $$}
    pred {TPred $$}
    "->" {TImpl}
    "|"  {TDisj}
    "&"  {TConj}
    "!"  {TInv}
    "?"  {TExist}
    "@"  {TForAll}
    "("  {TLeftPar}
    ")"  {TRightPar}
    ","  {TComma}
    "0"  {TZero}
    "'"  {TSucc}
    "+"  {TAdd}
    "*"  {TMult}
    "="  {TEq}
    "|-" {TCarret}
%%
Context:
    "|-" Expr {([], $2)}
    | Exprs "|-" Expr {($1, $3)}
Exprs:
    Expr "," Exprs {$1 : $3}
    | Expr {[$1]}
Expr:
    Disj {$1}
    | Disj "->" Expr {Impl $1 $3}
Disj:
    Conj {$1}
    | Disj "|" Conj {Disj $1 $3}
Conj:
    Unary {$1}
    | Conj "&" Unary {Conj $1 $3}
Unary:
    Predicate {$1}
    | "!" Unary {Inv $2}
    | "(" Expr ")" {$2}
    | "?" var Unary {Exist (Var $2) $3}
    | "@" var Unary {ForAll (Var $2) $3}
Predicate:
    pred {Variable $1}
    | pred "(" Terms ")" {Predicate $1 $3}
    | Term "=" Term    {Eq $1 $3}
Terms:
    Term "," Terms {$1: $3}
    | Term {[$1]}
Term:
    Term "+" Addend {Add $1 $3}
    | Addend {$1}
Addend:
    Multiplicand {$1}
    | Addend "*" Multiplicand {Mult $1 $3}
Multiplicand:
    "0" {Zero}
    | Multiplicand "'" {Succ $1}
    | "(" Term ")" {$2}
    | var "(" Terms ")" {Func $1 $3}
    | var {Var $1}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Expr = Impl Expr Expr | Disj Expr Expr | Conj Expr Expr | Inv Expr | Exist Term Expr | ForAll Term Expr | Predicate String [Term] | Eq Term Term | Variable String deriving (Generic, Eq, Ord)

data Term = Add Term Term | Mult Term Term | Succ Term | Func String [Term] | Zero | Var String deriving (Generic, Eq, Ord)

parse x = parser $ alexScanTokens x

}