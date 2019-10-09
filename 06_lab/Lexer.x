{
module Lexer where
}

%wrapper "basic"

tokens :-
    $white+             ;
    \n                  ;
    "."                   ;
    [A-Z]+[0-9]*        { \s -> TPred s }
    [a-z]+[0-9]*        { \s -> TVar s }
    "->"                { \s -> TImpl }
    "|"                 { \s -> TDisj }
    "&"                 { \s -> TConj }
    "!"                 { \s -> TInv }
    "?"                 { \s -> TExist }
    "@"                 { \s -> TForAll }
    "("                 { \s -> TLeftPar }
    ")"                 { \s -> TRightPar }
    ","                 { \s -> TComma }
    "0"                 { \s -> TZero }
    "'"                 { \s -> TSucc }
    "+"                 { \s -> TAdd }
    "*"                 { \s -> TMult }
    "="                 { \s -> TEq }
    "|-"                { \s -> TCarret}
{
data Token = TPred !String | TVar !String | TCarret | TImpl | TDisj | TConj | TInv | TExist | TForAll | TLeftPar | TRightPar | TComma | TZero | TSucc | TAdd | TMult | TEq deriving (Eq, Show)
}