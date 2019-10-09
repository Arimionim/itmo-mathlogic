module Main where

import Check
import Lexer
import Parser
import Grammar
import System.IO
import Data.List as List

writePrefix :: ExprType -> Int -> IO ()
writePrefix (Axiom a) n = putStr ("["++(show n)++". Ax. sch. "++(show a)++"] ")
writePrefix (Hyp a) n = putStr ("["++(show n)++". Hypothesis "++(show a)++"] ")
writePrefix (MP a b) n = putStr ("["++(show n)++". M.P. "++(show a)++", "++(show b)++"] ")
writePrefix (Useless) n = putStr ("Useless: "++(show n))
writePrefix (Incorrect) n = putStr ("Incorrect: "++(show n))


writeLine :: Expr -> Int -> ExprType -> IO ()
writeLine exp n typ | otherwise = do
	writePrefix typ n
	print exp
--					| otherwise = putStr ("")


writeAns :: [Expr] -> [ExprType] -> IO ()
writeAns exprs types = writeAnsHelper exprs types 1 where
	writeAnsHelper :: [Expr] -> [ExprType] -> Int -> IO ()
	writeAnsHelper (expr : exprTail) (typ : typeTail) !n = do
		writeLine expr n typ
		writeAnsHelper exprTail typeTail (n + 1)
	writeAnsHelper [] [] _ = do
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
	let expr_prooving = last headParsed
	let hyps = init headParsed
	src <- getContents
	let tokens = map alexScanTokens (lines src)
	let firstProof = map parseExpr tokens
	let firstChecked = check firstProof hyps
	if (any (== Incorrect) firstChecked || isEmpty firstProof || last firstProof /= expr_prooving) then
		putStrLn "Proof is incorrect"
	else do
		putStrLn headLine
		let cutProove = findFirstProoving expr_prooving firstProof 
		let cutTypes = cutTypesFun cutProove firstChecked
		let minimized = List.reverse(minimize cutProove cutTypes (List.length cutProove))
		let sndChecked = check minimized hyps
		writeAns minimized sndChecked 