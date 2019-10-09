module Check where
import Parser
import Lexer
import Grammar
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Maybe

numberAx :: Expr -> Int
numberAx (Binary Impl a (Binary Impl b a1)) | a1 == a = 1
numberAx (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a1 (Binary Impl b1 c)) (Binary Impl a2 c1))) | a == a1 && a == a2 && b == b1 && c == c1 = 2
numberAx (Binary Impl a (Binary Impl b (Binary And a1 b1))) | a == a1 && b == b1 = 3
numberAx (Binary Impl (Binary And a b) a1) | a == a1 = 4
numberAx (Binary Impl (Binary And b a) a1) | a == a1 = 5
numberAx (Binary Impl a (Binary Or a1 b)) | a == a1 = 6
numberAx (Binary Impl a (Binary Or b a1)) | a == a1 = 7
numberAx (Binary Impl (Binary Impl a c) (Binary Impl (Binary Impl b c1) (Binary Impl (Binary Or a1 b1) c2))) | a == a1 && b == b1 && c == c1 && c == c2 = 8;
numberAx (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a1 (Not b1)) (Not a2))) | a == a1 && a == a2 && b == b1 = 9
numberAx (Binary Impl (Not (Not a)) a1) | a == a1 = 10
numberAx _ = (-1)

data ExprType = Incorrect | MP Int Int | Hyp Int | Axiom Int | Useless deriving (Show, Eq)

checkMP :: Expr -> Map.Map Expr Int -> Map.Map Expr [(Expr, Int)] -> ExprType
checkMP expr prooved btoa | canProove /= ((-1), (-1)) = makeMP canProove
						  | otherwise = Incorrect
	where
		canProove | Map.member expr btoa = findProoved (btoa Map.! expr)
				  | otherwise = ((-1), (-1))
		findProoved ((a, n) : tail) | Map.member a prooved = (n, prooved Map.! a)
									| otherwise = findProoved tail
		findProoved [] = ((-1), (-1))
		makeMP (a, b) = MP a b
		makeMP _ = Incorrect

updateBtoA expr btoa n | isImpl expr = addHelper expr
				       | otherwise = btoa
	where
		push_back (a : []) l = a : l
		addHelper (Binary Impl a b) = Map.insertWith (push_back) b [(a, n)] btoa
		isImpl (Binary Impl a b) = True
		isImpl _ = False

check :: [Expr] -> [Expr] -> [ExprType]
check inputList hyps = checkHelper inputList 1 Map.empty Map.empty where
	checkHelper (expr : exprTail) n prooved btoa | numberAx expr /= -1 = (Axiom $ numberAx expr) : checkHelper exprTail (n + 1) (Map.insert expr n prooved) (updateBtoA expr btoa n)
												  | any (== expr) hyps = (Hyp $ 1 + fromMaybe 0 (elemIndex expr hyps)) : checkHelper exprTail (n + 1) (Map.insert expr n prooved) (updateBtoA expr btoa n)
												  | otherwise = (checkMP expr prooved btoa) : (checkHelper exprTail (n + 1) (Map.insert expr n prooved) (updateBtoA expr btoa n))
	checkHelper [] _ _ _ = []

isHypOrAx (Hyp a) = True
isHypOrAx (Axiom a) = True
isHypOrAx _ = False

isTenthAx (Axiom a) | a == 10 = True
					| otherwise = False
isTenthAx _ = False

isMP (MP a b) = (a, b)
isMP _ = (-1, -1)

getSndFromImpl (Binary Impl a b) = b
getSndFromImpl _ = undefined

minimize :: [Expr] -> [ExprType] -> Int -> [Expr]
minimize proove types n = minimizeHelper (List.reverse proove) (List.reverse types) n (Set.singleton n) where
	minimizeHelper :: [Expr] -> [ExprType] -> Int -> Set.Set Int -> [Expr]
	minimizeHelper (expr : exprTail) (typ : typTail) n need
		| not (Set.member n need) = minimizeHelper exprTail typTail (n - 1) need 
		| typ == Useless = minimizeHelper exprTail typTail (n - 1) need
		| isHypOrAx typ = expr : minimizeHelper exprTail typTail (n - 1) (Set.delete n need)
		| isMP typ /= (-1, -1) = expr : minimizeHelper exprTail typTail (n - 1) (Set.delete n (Set.insert (fst (isMP typ)) (Set.insert (snd (isMP typ)) need)))
	minimizeHelper [] [] _ _ = []

findFirstProoving expr (e: exprTail) | expr == e = [expr]
									 | otherwise = e : findFirstProoving expr exprTail

cutTypesFun (expr : exprTail) (typ: typeTail) = typ : cutTypesFun exprTail typeTail
cutTypesFun [] _ = []