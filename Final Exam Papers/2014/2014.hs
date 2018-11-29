-- Informatics 1 Functional Programming
-- December 2014
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck,
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM2, liftM3, used below
import Data.Char

-- Question 1

-- 1a

f :: [Int] -> Bool
f (x:xs) = and [if y `mod` x == 0 then True else False | (x,y) <- zip (x:xs) xs]

-- 1b

g :: [Int] -> Bool
g [x] = True
g (x:y:xs)
    | y `mod` x == 0 = True && g (y:xs)
    | otherwise = False

-- Question 2

-- 2a

p :: [Int] -> Int
p xs = product [x^2| x <- xs, x < 0]

test_p = (p [13] == 1) && (p [] == 1) && (p [-3,3,1,-3,2,-1] == 81) && (p [2,6,-3,0,3,-7,2] == 441) && (p [4,-2,-1,-3] == 36)
-- 2b

q :: [Int] -> Int
q [] = 1
q (x:xs)
    | x < 0 = (x^2) * q xs
    | otherwise = q xs

test_q = (q [13] == 1) && (q [] == 1) && (q [-3,3,1,-3,2,-1] == 81) && (q [2,6,-3,0,3,-7,2] == 441) && (q [4,-2,-1,-3] == 36)

-- 2c

r :: [Int] -> Int
r xs = foldr (*) 1 (map (^2) (filter (<0) xs))

-- Question 3

data Expr = X
          | Const Int
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | IfZero Expr Expr Expr
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :-: q)  =  "(" ++ showExpr p ++ "-" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"
showExpr (p :/: q)  =  "(" ++ showExpr p ++ "/" ++ showExpr q ++ ")"
showExpr (IfZero p q r)  = "(if " ++ showExpr p ++ "=0 then "
                                  ++ showExpr q ++ " else "
                                  ++ showExpr r ++ ")"

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [X]]
                 | otherwise  =  oneof [ liftM Const arbitrary
                                       , liftM2 (:+:) subform2 subform2
                                       , liftM2 (:-:) subform2 subform2
                                       , liftM2 (:*:) subform2 subform2
                                       , liftM2 (:/:) subform2 subform2
                                       , liftM3 (IfZero) subform3 subform3 subform3
                                       ]
                 where
                   subform2  =  expr (n `div` 2)
                   subform3  =  expr (n `div` 3)

-- 3a

eval :: Expr -> Int -> Int
eval X v = v
eval (Const n) _ = n
eval (p :+: q) v = (eval p v) + (eval q v)
eval (p :-: q) v = (eval p v) - (eval q v)
eval (p :*: q) v = (eval p v) * (eval q v)
eval (p :/: q) v = (eval p v) `div` (eval q v)
eval (IfZero p q r) v = if (eval p v)==0 then eval q v else eval r v

-- 3 b

protect :: Expr -> Expr
protect X = X
protect (Const n) = (Const n)
protect (p :+: q) = (protect p) :+: (protect q)
protect (p :-: q) = (protect p) :-: (protect q)
protect (p :*: q) = (protect p) :*: (protect q)
protect (p :/: q) = IfZero (protect p) (Const maxBound) ((protect p) :/: (protect q))
protect (IfZero p q r) = IfZero (protect p) (protect q) (protect r)
