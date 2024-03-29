module Mmm where

import Parser
import Lexer
import Grammar

printHyp :: Expr -> IO ()
printHyp a = do 
    print a
    print (Binary Impl a (Binary Impl (Not a) a))
    print (Binary Impl (Not a) a)
    print (Binary Impl (Not a) (Binary Impl (Not a) (Not a)))
    print (Binary Impl (Binary Impl (Not a) (Binary Impl (Not a) (Not a))) (Binary Impl (Binary Impl (Not a) (Binary Impl (Binary Impl (Not a) (Not a)) (Not a))) (Binary Impl (Not a) (Not a))))
    print (Binary Impl (Binary Impl (Not a) (Binary Impl (Binary Impl (Not a) (Not a)) (Not a))) (Binary Impl (Not a) (Not a)))
    print (Binary Impl (Not a) (Binary Impl (Binary Impl (Not a) (Not a)) (Not a)))
    print (Binary Impl (Not a) (Not a))
    print (Binary Impl (Binary Impl (Not a) a) (Binary Impl (Binary Impl (Not a) (Not a)) (Not (Not a))))
    print (Binary Impl (Binary Impl (Not a) (Not a)) (Not (Not a)))
    print (Not (Not a))

printAx10 :: Expr -> IO ()
printAx10 a = do
    print (Binary Impl a (Binary Impl (Not (Not a)) a))
    print (Binary Impl (Binary Impl a (Binary Impl (Not (Not a)) a)) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl a (Binary Impl (Not (Not a)) a))))
    print (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl a (Binary Impl (Not (Not a)) a)))
    print (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl a (Not (Binary Impl (Not (Not a)) a))))
    print (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not (Binary Impl (Not (Not a)) a))))
    print (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not (Binary Impl (Not (Not a)) a))) (Not (Binary Impl (Not (Not a)) a))))
    print (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not (Binary Impl (Not (Not a)) a)))) (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not (Binary Impl (Not (Not a)) a))) (Not (Binary Impl (Not (Not a)) a)))) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not (Binary Impl (Not (Not a)) a)))))
    print (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not (Binary Impl (Not (Not a)) a))) (Not (Binary Impl (Not (Not a)) a)))) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not (Binary Impl (Not (Not a)) a))))
    print (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not (Binary Impl (Not (Not a)) a)))
    print (Binary Impl (Binary Impl a (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Not (Binary Impl (Not (Not a)) a))) (Not a)))
    print (Binary Impl (Binary Impl (Binary Impl a (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Not (Binary Impl (Not (Not a)) a))) (Not a))) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Not (Binary Impl (Not (Not a)) a))) (Not a)))))
    print (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Not (Binary Impl (Not (Not a)) a))) (Not a))))
    print (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl a (Binary Impl (Not (Not a)) a))) (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Not (Binary Impl (Not (Not a)) a))) (Not a)))) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Not (Binary Impl (Not (Not a)) a))) (Not a)))))
    print (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Not (Binary Impl (Not (Not a)) a))) (Not a)))) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Not (Binary Impl (Not (Not a)) a))) (Not a))))
    print (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Not (Binary Impl (Not (Not a)) a))) (Not a)))
    print (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl a (Not (Binary Impl (Not (Not a)) a)))) (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Not (Binary Impl (Not (Not a)) a))) (Not a))) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not a))))
    print (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl a (Not (Binary Impl (Not (Not a)) a))) (Not a))) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not a)))
    print (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not a))
    print (Binary Impl (Not a) (Binary Impl (Not (Not a)) a))
    print (Binary Impl (Binary Impl (Not a) (Binary Impl (Not (Not a)) a)) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Not a) (Binary Impl (Not (Not a)) a))))
    print (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Not a) (Binary Impl (Not (Not a)) a)))
    print (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not a)) (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Not a) (Binary Impl (Not (Not a)) a))) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Not (Not a)) a))))
    print (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Not a) (Binary Impl (Not (Not a)) a))) (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Not (Not a)) a)))
    print (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Not (Not a)) a))
    print (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Binary Impl (Not (Not a)) a)) (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not (Binary Impl (Not (Not a)) a))) (Not (Not (Binary Impl (Not (Not a)) a)))))
    print (Binary Impl (Binary Impl (Not (Binary Impl (Not (Not a)) a)) (Not (Binary Impl (Not (Not a)) a))) (Not (Not (Binary Impl (Not (Not a)) a))))
    print (Not (Not (Binary Impl (Not (Not a)) a)))

printMP :: Expr -> Expr -> IO ()
printMP a b = do
    print (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not b)))
    print (Binary Impl (Not b) (Binary Impl a (Not b)))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl a (Not b))) (Binary Impl (Not b) (Binary Impl (Not b) (Binary Impl a (Not b)))))
    print (Binary Impl (Not b) (Binary Impl (Not b) (Binary Impl a (Not b))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl a (Not b))) (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))))
    print (Binary Impl (Binary Impl (Binary Impl (Not b) (Binary Impl a (Not b))) (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Not b) (Binary Impl a (Not b))) (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl (Not b) (Binary Impl a (Not b))) (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Not b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Not b) (Binary Impl a (Not b))) (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Not b) (Binary Impl a (Not b))) (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))))
    print (Binary Impl (Binary Impl (Binary Impl a b) (Not b)) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b)))))
    print (Binary Impl (Binary Impl (Binary Impl (Binary Impl a b) (Not b)) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not b)) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b)))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not b)) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not b))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not b)) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b)))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b)))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not b)) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b)))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b)))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b))))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b)))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))))
    print (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a)))
    print (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a)))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))))
    print (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))) (Binary Impl (Binary Impl a b) (Not a))))
    print (Binary Impl (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))) (Binary Impl (Binary Impl a b) (Not a)))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))) (Binary Impl (Binary Impl a b) (Not a))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))) (Binary Impl (Binary Impl a b) (Not a)))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl a (Not b)))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))) (Binary Impl (Binary Impl a b) (Not a))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))) (Binary Impl (Binary Impl a b) (Not a))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl a (Not b))) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))) (Binary Impl (Binary Impl a b) (Not a))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))) (Binary Impl (Binary Impl a b) (Not a)))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))) (Binary Impl (Binary Impl a b) (Not a))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a)))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))) (Binary Impl (Binary Impl a b) (Not a)))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not a)))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a (Not b)) (Not a))) (Binary Impl (Binary Impl a b) (Not a)))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not a))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not a)))
    print (Not (Not a))
    print (Binary Impl (Not (Not a)) (Binary Impl (Not b) (Not (Not a))))
    print (Binary Impl (Not b) (Not (Not a)))
    print (Binary Impl (Not (Not a)) (Binary Impl (Binary Impl a b) (Not (Not a))))
    print (Binary Impl (Binary Impl (Not (Not a)) (Binary Impl (Binary Impl a b) (Not (Not a)))) (Binary Impl (Not b) (Binary Impl (Not (Not a)) (Binary Impl (Binary Impl a b) (Not (Not a))))))
    print (Binary Impl (Not b) (Binary Impl (Not (Not a)) (Binary Impl (Binary Impl a b) (Not (Not a)))))
    print (Binary Impl (Binary Impl (Not b) (Not (Not a))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Not (Not a)) (Binary Impl (Binary Impl a b) (Not (Not a))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not (Not a))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Not (Not a)) (Binary Impl (Binary Impl a b) (Not (Not a))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not (Not a)))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not (Not a))))
    print (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))
    print (Binary Impl (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Not b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))
    print (Binary Impl (Not b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))))
    print (Binary Impl (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))
    print (Binary Impl (Binary Impl (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))
    print (Binary Impl (Binary Impl (Binary Impl a b) (Not a)) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))
    print (Binary Impl (Binary Impl (Binary Impl (Binary Impl a b) (Not a)) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not a)) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not a)) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not a))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not a)) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not a)) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not a) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))))
    print (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not a))) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b)))))
    print (Binary Impl (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not a))) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not a))) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b)))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not a))) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not (Not a)))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not a))) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b)))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b)))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not a))) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b)))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b)))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b))))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Binary Impl (Not (Not a)) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b)))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))))
    print (Not (Not (Binary Impl a b)))
    print (Binary Impl (Not (Not (Binary Impl a b))) (Binary Impl (Not b) (Not (Not (Binary Impl a b)))))
    print (Binary Impl (Not b) (Not (Not (Binary Impl a b))))
    print (Binary Impl (Not (Not (Binary Impl a b))) (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))))
    print (Binary Impl (Binary Impl (Not (Not (Binary Impl a b))) (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b))))) (Binary Impl (Not b) (Binary Impl (Not (Not (Binary Impl a b))) (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))))))
    print (Binary Impl (Not b) (Binary Impl (Not (Not (Binary Impl a b))) (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b))))))
    print (Binary Impl (Binary Impl (Not b) (Not (Not (Binary Impl a b)))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Not (Not (Binary Impl a b))) (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Not (Not (Binary Impl a b))) (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))))) (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))))
    print (Binary Impl (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))) (Not (Binary Impl a b))))
    print (Binary Impl (Binary Impl (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))) (Not (Binary Impl a b)))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))) (Not (Binary Impl a b))))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))) (Not (Binary Impl a b)))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not (Binary Impl a b)))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))) (Not (Binary Impl a b))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))) (Not (Binary Impl a b))))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Binary Impl a b))) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))) (Not (Binary Impl a b))))) (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))) (Not (Binary Impl a b)))))
    print (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))) (Not (Binary Impl a b))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b))))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))) (Not (Binary Impl a b)))) (Binary Impl (Not b) (Not (Binary Impl a b)))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Binary Impl (Binary Impl a b) (Not (Not (Binary Impl a b)))) (Not (Binary Impl a b)))) (Binary Impl (Not b) (Not (Binary Impl a b))))
    print (Binary Impl (Not b) (Not (Binary Impl a b)))
    print (Binary Impl (Not (Binary Impl a b)) (Binary Impl (Not (Not (Binary Impl a b))) (Not a)))
    print (Binary Impl (Binary Impl (Not (Binary Impl a b)) (Binary Impl (Not (Not (Binary Impl a b))) (Not a))) (Binary Impl (Not b) (Binary Impl (Not (Binary Impl a b)) (Binary Impl (Not (Not (Binary Impl a b))) (Not a)))))
    print (Binary Impl (Not b) (Binary Impl (Not (Binary Impl a b)) (Binary Impl (Not (Not (Binary Impl a b))) (Not a))))
    print (Binary Impl (Binary Impl (Not b) (Not (Binary Impl a b))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Not (Binary Impl a b)) (Binary Impl (Not (Not (Binary Impl a b))) (Not a)))) (Binary Impl (Not b) (Binary Impl (Not (Not (Binary Impl a b))) (Not a)))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Not (Binary Impl a b)) (Binary Impl (Not (Not (Binary Impl a b))) (Not a)))) (Binary Impl (Not b) (Binary Impl (Not (Not (Binary Impl a b))) (Not a))))
    print (Binary Impl (Not b) (Binary Impl (Not (Not (Binary Impl a b))) (Not a)))
    print (Binary Impl (Binary Impl (Not b) (Not (Not (Binary Impl a b)))) (Binary Impl (Binary Impl (Not b) (Binary Impl (Not (Not (Binary Impl a b))) (Not a))) (Binary Impl (Not b) (Not a))))
    print (Binary Impl (Binary Impl (Not b) (Binary Impl (Not (Not (Binary Impl a b))) (Not a))) (Binary Impl (Not b) (Not a)))
    print (Binary Impl (Not b) (Not a))
    print (Binary Impl (Binary Impl (Not b) (Not a)) (Binary Impl (Binary Impl (Not b) (Not (Not a))) (Not (Not b))))
    print (Binary Impl (Binary Impl (Not b) (Not (Not a))) (Not (Not b)))
    print (Not (Not b))