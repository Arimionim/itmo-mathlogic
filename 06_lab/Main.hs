module Main where

import Solve
import System.IO
import Data.List as List
import Parser



showbinop s a b = "(" ++ show a ++ s ++ show b ++ ")"

showfunc s t = s ++ "(" ++ (intercalate ", " $ map show t) ++ ")"

instance Show Term where
  show Zero = "0"
  show (Succ t) = show t ++ "'"
  show (Add a b) = showbinop "+" a b
  show (Mult a b) = showbinop "*" a b
  show (Var s) = s
  show (Func s t) = showfunc s t

instance Show Expr where
  show (Impl a b) = showbinop "->" a b
  show (Disj a b) = showbinop "|" a b
  show (Conj a b) = showbinop "&" a b
  show (Inv a) = "!" ++ show a
  show (Exist a b) = "(?" ++ show a ++ ".(" ++ show b ++ "))"
  show (ForAll a b) = "(@" ++ show a ++ ".(" ++ show b ++ "))"
  show (Variable a) = a
  show (Eq a b) = showbinop "=" a b
  show (Predicate s t) = showfunc s t


writeLines lines = writeAnsHelper lines where
	writeAnsHelper (expr : exprTail) = do
		let s = parse expr
		putStr ((show s)++"\n")
		writeAnsHelper(exprTail)
	writeAnsHelper ([]) = return () 


axis = 
	[
	"(a=b)->(a'=b')",
	"((a=b)->(a'=b'))->((0=0)->(0=0)->(0=0))->((a=b)->(a'=b'))",
	"((0=0)->(0=0)->(0=0))->((a=b)->(a'=b'))",
	"((0=0)->(0=0)->(0=0))->@a.((a=b)->(a'=b'))",
	"((0=0)->(0=0)->(0=0))->@b.@a.((a=b)->(a'=b'))",
	"(0=0)->(0=0)->(0=0)",
	"(@b.(@a.((a=b)->(a'=b'))))",

	"(a=b)->(a=c)->(b=c)",
	"((a=b)->(a=c)->(b=c))->((0=0)->(0=0)->(0=0))->((a=b)->(a=c)->(b=c))",
	"((0=0)->(0=0)->(0=0))->((a=b)->(a=c)->(b=c))",
	"((0=0)->(0=0)->(0=0))->@a.((a=b)->(a=c)->(b=c))",
	"((0=0)->(0=0)->(0=0))->@b.@a.((a=b)->(a=c)->(b=c))",
	"((0=0)->(0=0)->(0=0))->@c.@b.@a.((a=b)->(a=c)->(b=c))",
	"(0=0)->(0=0)->(0=0)",
	"(@c.(@b.(@a.((a=b)->(a=c)->(b=c)))))",
	
	"(a'=b')->(a=b)",
	"((a'=b')->(a=b))->((0=0)->(0=0)->(0=0))->((a'=b')->(a=b))",
	"((0=0)->(0=0)->(0=0))->((a'=b')->(a=b))",
	"((0=0)->(0=0)->(0=0))->@a.((a'=b')->(a=b))",
	"((0=0)->(0=0)->(0=0))->@b.@a.((a'=b')->(a=b))",
	"(0=0)->(0=0)->(0=0)",
	"(@b.(@a.((a'=b')->(a=b))))",
	
	"!(a'=0)",
    "!(a'=0)->((0=0)->(0=0)->(0=0))->!(a'=0)",
    "((0=0)->(0=0)->(0=0))->!(a'=0)",
    "((0=0)->(0=0)->(0=0))->@a.!(a'=0)",
    "(0=0)->(0=0)->(0=0)",
	"(@a.(!(a'=0)))",
	
	"(a+b')=(a+b)'",
    "((a+b')=(a+b)')->((0=0)->(0=0)->(0=0))->((a+b')=(a+b)')",
    "((0=0)->(0=0)->(0=0))->((a+b')=(a+b)')",
    "((0=0)->(0=0)->(0=0))->@a.((a+b')=(a+b)')",
    "((0=0)->(0=0)->(0=0))->@b.@a.((a+b')=(a+b)')",
    "(0=0)->(0=0)->(0=0)",
	"(@b.(@a.((a+b')=(a+b)')))",

	"a+0=a",
	"(a+0=a)->((0=0)->(0=0)->(0=0))->(a+0=a)",
	"((0=0)->(0=0)->(0=0))->(a+0=a)",
	"((0=0)->(0=0)->(0=0))->@a.(a+0=a)",
	"(0=0)->(0=0)->(0=0)",
	"(@a.(a+0=a))",

	"a*0=0",
	"(a*0=0)->((0=0)->(0=0)->(0=0))->(a*0=0)",
	"((0=0)->(0=0)->(0=0))->(a*0=0)",
	"((0=0)->(0=0)->(0=0))->@a.(a*0=0)",
	"(0=0)->(0=0)->(0=0)",
	"(@a.(a*0=0))",

	"(a*b')=((a*b)+a)",
	"((a*b')=((a*b)+a))->((0=0)->(0=0)->(0=0))->((a*b')=((a*b)+a))",
	"((0=0)->(0=0)->(0=0))->((a*b')=((a*b)+a))",
	"((0=0)->(0=0)->(0=0))->@a.((a*b')=((a*b)+a))",
	"((0=0)->(0=0)->(0=0))->@b.@a.((a*b')=((a*b)+a))",
	"(0=0)->(0=0)->(0=0)",
	"(@b.(@a.((a*b')=((a*b)+a))))",

	"(@a.(!(a'=0)))->(!(y'=0))"
	]
			

main :: IO ()
main = do
	headLine <- getLine
	let headSplitted = split headLine ' '
	let a = read (head headSplitted) :: Int
	let b = read (last headSplitted) :: Int
	let solution = solve a b
	putStr ("|- "++(last solution)++"\n")
	writeLines axis
	writeLines solution
