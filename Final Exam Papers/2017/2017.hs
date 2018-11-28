import Test.QuickCheck
import Data.Char

-- Question 1
-- a
f :: [Int] -> [Int]
f [] = []
f (x:xs) = [y - x | (x,y) <- zip (x:xs) xs, y > x]

test1a = f [4,2,5,6,1,8] == [3,1,7] && f [] == [] && f [3] == [] && f [3,3,1,-3] == []
-- b
g :: [Int] -> [Int]
g [] = []
g [x] = []
g (x:y:xs)
    | x < y = y-x : g (y:xs)
    | otherwise = g (y:xs)

prop_fg :: [Int] -> Bool
prop_fg xs = f xs == g xs

-- Question 2
-- a
isInitialism :: String -> Bool
isInitialism x = length x > 1 && and [isUpper c | c <- x]

p :: [String] -> Int
p xs = sum [length x | x <- xs, isInitialism x]

-- b
isInitialism' :: String -> Bool
isInitialism' [] = False
isInitialism' [s] = False
isInitialism' ss = and (caps (ss))

caps :: String -> [Bool]
caps [] = [True]
caps (x:xs)
    | isUpper x = True : caps (xs)
    | otherwise = [False]

q :: [String] -> Int
q [] = 0
q (x:xs)
    | isInitialism x = length x + q xs
    | otherwise = q xs

-- c

r :: [String] -> Int
r xs = foldr (+) 0 (map length (filter isInitialism' xs))

-- Question 3
data Expr = X
          | Const Int
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Ord)

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- 3 a

eval :: Expr -> Int -> Int
eval X i =i
eval (Const n) _ = n
eval (p:+:q) i = eval p i + eval q i
eval (p:*:q) i = eval p i * eval q i
