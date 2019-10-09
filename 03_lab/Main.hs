module Main where

import Check
import Lexer
import Parser
import Grammar
import Mmm
import System.IO
import Data.List as List

writeLine :: Expr -> Int -> ExprType -> [Expr] -> IO ()
writeLine exp n typ full | isTenthAx typ = do
						 	printAx10 $ getSndFromImpl exp
		  				 | isHypOrAx typ = do
							printHyp exp
						 | isMP typ /= (-1, -1) = do
								printMP (getExpr (snd (isMP typ)) full) exp
						 | typ == Incorrect = do
							putStrLn("Incorrect: ")
						 | typ == Useless = do
							putStrLn("Useless: ") where
								getExpr :: Int -> [Expr] -> Expr
								getExpr n full = getExprHelper n full
								getExprHelper n (expr: exprTail)   | n == 1 = expr
																   | otherwise = getExprHelper (n - 1) exprTail

writeAns :: [Expr] -> [ExprType] -> IO ()
writeAns exprs types = writeAnsHelper exprs types 1 exprs where
	writeAnsHelper :: [Expr] -> [ExprType] -> Int -> [Expr] -> IO ()
	writeAnsHelper (expr : exprTail) (typ : typeTail) n full = do
		writeLine expr n typ full
		writeAnsHelper exprTail typeTail (n + 1) full
	writeAnsHelper [] [] _ _ = do
		return ()

split :: String -> [String]

split [] = [""]
split (a:b:cs) | a == ',' = "" : split (b : cs)
               | a == '|' && b == '-' = "" : split cs
               | otherwise = (a : head rest) : tail rest
               where
                    rest = split (b : cs) 

split (a:b) = [(a:b)]

isEmpty [] = True
isEmpty _ = False

main :: IO ()
main = do
	headLine <- getLine
	
	let headSplitted = split headLine
	let headList = filter (/= []) (map alexScanTokens headSplitted)

	let headParsed = map parseExpr headList
	let expr_prooving = Not (Not (last headParsed))
	let hyps = init headParsed
	src <- getContents
	let tokens = map alexScanTokens (lines src)
	let firstProof = map parseExpr tokens
	let firstChecked = check firstProof hyps
	putStr $ intercalate ", " $ map show hyps
	putStr " |- "
	print expr_prooving
	writeAns firstProof firstChecked