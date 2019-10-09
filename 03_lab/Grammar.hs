module Grammar where

    import Data.List (intercalate)
    
    data Binop = Or | And | Impl deriving (Eq, Ord)
    
    instance Show Binop where
      show Or   = "|"
      show And  = "&"
      show Impl = "->"
    
    data Expr = Binary Binop Expr Expr
              | Not Expr
              | Var String
              deriving (Eq, Ord)
    
    instance Show Expr where
      show (Binary op a b) = "(" ++ intercalate " " [show a, show op, show b] ++ ")"
      show (Not e)         = "!" ++ show e
      show (Var s)         = s
    