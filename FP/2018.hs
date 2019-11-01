-- Informatics 1 Functional Programming
-- August 2018

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ), Gen, suchThat,
                        oneof, elements, sized, (==>) )
import Control.Monad -- defines liftM, liftM2, liftM3, used below
import Data.Char

-- Question 1

f :: [String] -> [String]
f strs = [(last two) : (tail one) | (one,two) <- zip strs (tail strs)]

test_f = 
       f ["pattern","matching","rules","ok"] == ["gattern","satching","kules"]
    && f ["word"]                            == []   
    && f ["almost","all","students","love","functional","programming"] == ["llmost","sll","etudents","love","gunctional"]
    && f ["make","love","not","war"]         == ["eake","tove","rot"] 

g :: [String] -> [String]
g [] = []
g [_] = []
g (x:y:xs) = (last y : tail x) : g (y : xs)

test_g = 
    g ["pattern","matching","rules","ok"] == ["gattern","satching","kules"]
 && g ["word"]                            == []   
 && g ["almost","all","students","love","functional","programming"] == ["llmost","sll","etudents","love","gunctional"]
 && g ["make","love","not","war"]         == ["eake","tove","rot"]


-- Question 2

-- 2a
isTLA :: String -> Bool
isTLA [a,b,c] = isUpper a && isUpper b && isUpper c
isTLA _ = False


p :: [String] -> Int
p strs = length [s | s <- strs, isTLA s]
    

test_p = 
    p ["I","played","the","BBC","DVD","in","the","USA"] == 3
 && p ["The","DUP","MP","travelled","to","LHR"] == 2
 && p ["The","SNP","won","in","South","Morningside"] == 1
 && p [] == 0 

-- 2b

q :: [String] -> Int
q [] = 0
q (x:xs)
    | isTLA x   = 1 + q xs
    | otherwise =     q xs


test_q = 
    q ["I","played","the","BBC","DVD","in","the","USA"] == 3
 && q ["The","DUP","MP","travelled","to","LHR"] == 2
 && q ["The","SNP","won","in","South","Morningside"] == 1
 && q [] == 0 

-- 2c

r :: [String] -> Int
r strs = foldr add 0 $ map value $ filter isTLA strs
    where
        add x y = x + y
        value _ = 1
        

test_r = 
    r ["I","played","the","BBC","DVD","in","the","USA"] == 3
 && r ["The","DUP","MP","travelled","to","LHR"] == 2
 && r ["The","SNP","won","in","South","Morningside"] == 1
 && r [] == 0 

-- Question 3

data Expr = X                      -- variable X
          | Y                      -- variable Y
          | Const Int              -- integer constant
          | Expr :+: Expr          -- addition
          | Expr :*: Expr          -- multiplication
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr Y          =  "Y"
showExpr (Const n)  =  show n
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [ return X
                                       , return Y
                                       , liftM Const arbitrary ]
                 | otherwise  =  oneof [ return X
                                       , return Y
                                       , liftM Const arbitrary
                                       , liftM2 (:+:) subform2 subform2
                                       , liftM2 (:*:) subform2 subform2
                                       ]
                 where
                   subform2  =  expr (n `div` 2)

-- 3a

eval :: Expr -> Int -> Int -> Int
eval (p :+: q) x y = eval p x y + eval q x y
eval (p :*: q) x y = eval p x y * eval q x y
eval X x _ = x
eval Y _ y = y
eval (Const a) _ _ = a

test_eval =
    eval ((X :*: Const 3) :+: (Const 0 :*: Y)) 2 4 == 6 
 && eval (X :*: (Const 3 :+: Y)) 2 4                == 14 
 && eval (Y :+: (Const 1 :*: X)) 3 2                == 5
 && eval (((Const 1 :*: Const 1) :*: (X :+: Const 1)) :*: Y) 3 4 == 16

-- 3b

isSimple :: Expr -> Bool
isSimple (Const 0 :*: _) = False
isSimple (_ :*: Const 0) = False
isSimple (Const 1 :*: _) = False
isSimple (_ :*: Const 1) = False
isSimple (p :+: q) = isSimple p && isSimple q
isSimple (p :*: q) = isSimple p && isSimple q
isSimple _ = True

test_simple = 
    isSimple ((X :*: Const 3) :+: (Const 0 :*: Y)) == False
 && isSimple (X :*: (Const 3 :+: Y))               == True
 && isSimple (Y :+: (Const 1 :*: X))               == False
 && isSimple (((Const 1 :*: Const 1) :*: (X :+: Const 1)) :*: Y) == False 

-- 3c

simplify :: Expr -> Expr
simplify (Const 0 :*: p) = Const 0
simplify (p :*: Const 0) = Const 0
simplify (Const 1 :*: p) = simplify p
simplify (p :*: Const 1) = simplify p
simplify (p :+: q) = simplify p :+: simplify q
simplify (p :*: q) = simplify (simplify p) :*: simplify (simplify q)
simplify p = p

test_simplify = 
    simplify ((X :*: Const 3) :+: (Const 0 :*: Y)) == (X :*: Const 3) :+: Const 0
 && simplify (X :*: (Const 3 :+: Y)) == X :*: (Const 3 :+: Y)
 && simplify (Y :+: (Const 1 :*: X)) == Y :+: X
 && simplify (((Const 1 :*: Const 1) :*: (X :+: Const 1)) :*: Y) == (X :+: Const 1) :*: Y