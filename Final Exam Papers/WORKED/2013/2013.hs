-- Informatics 1 Functional Programming
-- December 2013
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck,
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char

-- Question 1

-- 1a

f :: String -> Int
f xs = sum [digitToInt x * 3^i | (x,i) <- zip (reverse xs) [0..] ]

-- 1b

g :: String -> Int
g xs = ga 0 (reverse xs)
  where
    ga i [] = 0
    ga i (x:xs) = digitToInt x * 3^i + ga (i+1) xs

-- Question 2

-- 2a

p :: [Int] -> Bool
p [] = error "Empty list"
p (a:xs)
     | a == 0 = error "Divide by zero error"
     | otherwise = and [x `mod` a == 0 | x <- xs, x >= 0]

-- 2b

q :: [Int] -> Bool
q [] = error "Empty list"
q (a:xs)
    | a == 0 = error "Divide by zero error"
    | otherwise = o xs
    where
        o [] = True
        o (x:xs) = x `mod` a == 0 && o xs

-- 2c
divBy :: Int -> Int -> Bool
x `divBy` y = (x `mod` y == 0)

r :: [Int] -> Bool
r (a:xs) | a /= 0  = foldr (&&) True (map (`divBy` a) (filter (>= 0) xs))

-- Question 3

data Expr = X
          | Const Int
          | Neg Expr
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (Neg p)    =  "(-" ++ showExpr p ++ ")"
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- evaluate an Expr, given a value of X

evalExpr :: Expr -> Int -> Int
evalExpr X v          =  v
evalExpr (Const n) _  =  n
evalExpr (Neg p) v    =  - (evalExpr p v)
evalExpr (p :+: q) v  =  (evalExpr p v) + (evalExpr q v)
evalExpr (p :*: q) v  =  (evalExpr p v) * (evalExpr q v)

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [X]]
                 | otherwise  =  oneof [ liftM Const arbitrary
                                       , liftM Neg subform
                                       , liftM2 (:+:) subform subform
                                       , liftM2 (:*:) subform subform
                                       ]
                 where
                   subform  =  expr (n `div` 2)

-- 3a

rpn :: Expr -> [String]
rpn X = ["X"]
rpn (Const n) = [show n]
rpn (Neg p) = rpn p ++ ["-"]
rpn (p :+: q) = rpn p ++ rpn q ++ ["+"]
rpn (p :*: q) = rpn p ++ rpn q ++ ["*"]

-- 3 b

evalrpn :: [String] -> Int -> Int
evalrpn s n = the (foldl step [] s)
    where
      step (x:y:ys) "+"  =  (y+x):ys
      step (x:y:ys) "*"  =  (y*x):ys
      step (x:ys) "-"    =  (-x):ys
      step ys "X"        =  n:ys
      step ys m | all (\c -> isDigit c || c=='-') m =  (read m :: Int):ys
                | otherwise  =  error "ill-formed RPN"
      the :: [a] -> a
      the [x] = x
      the xs = error "ill-formed RPN"
