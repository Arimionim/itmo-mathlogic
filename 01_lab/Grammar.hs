module Grammar where

import Data.List (intercalate)

data Binop = Or | And | Impl 

instance Show Binop where
  show Or   = "|"
  show And  = "&"
  show Impl = "->"

data Expr = Binary Binop Expr Expr
          | Not Expr
          | Var String

instance Show Expr where
  show (Binary op a b) = "(" ++ intercalate "," [show op, show a, show b] ++ ")"
  show (Not e)         = "(!" ++ show e ++ ")"
  show (Var s)         = s
