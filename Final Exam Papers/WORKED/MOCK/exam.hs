-- Informatics 1 Functional Programming
-- Mock exam
--
-- PLEASE ENTER YOUR DETAILS HERE
--
-- Name: MATTHEW TIMMONS-BROWN
-- Matric: S1823424


import Data.Char
import Data.List
import Control.Monad
import Test.QuickCheck


-- Question 1
-- 1a.

f :: String -> Bool
b = "234567890AKQJ"
a = "AKQJ"
f xs = and [x `elem` a | x <- xs, x `elem` b]

test_1a = (f "ABCDE" == True) && (f "none here" == True) && (f "4 Aces" == False) && (f "01234" == False) && (f "" == True) && (f "1 Ace" == True)

-- 1b.

g :: String -> Bool
g "" = True
g (x:xs)
    | x `elem` a = True && g xs
    | x `elem` b = False
    | otherwise = True && g xs

test_1b = (g "ABCDE" == True) && (g "none here" == True) && (g "4 Aces" == False) && (g "01234" == False) && (g "" == True) && (g "1 Ace" == True)

-- 1c.

h :: String -> Bool
h xs = (filter (`elem` a) xs) == (filter (`elem` b) xs)

test_1c = (h "ABCDE" == True) && (h "none here" == True) && (h "4 Aces" == False) && (h "01234" == False) && (h "" == True) && (h "1 Ace" == True)

-- Question 2
-- 2a.

--t :: [a] -> [a]
--t xs = [if y `mod` 2 == 0 then x else doubler x| x <- xs, y <- [2,4..]]

--doubler :: [a] -> a
--doubler x = concat [[x], [x]]

t :: [a] -> [a]
t xs = concat [ if odd i then [x] else [x,x] | (x,i) <- zip xs [1..] ]

test_2a =
    t "abcdefg" == "abbcddeffg"   &&
    t [1,2,3,4] == [1,2,2,3,4,4]  &&
    t ""        == ""

-- 2b.

--u :: [a] -> [a]
--u [] = []
--u (x:xs)
--    | isEven (length xs) = x : x : u xs
--    | otherwise = x : u xs

u :: [a] -> [a]
u [] = []
u [x] = [x]
u (x:y:xs) = x : y : y : u xs

--isEven :: Int -> Bool
--isEven x = x `mod` 2 == 0

test_2b =
    u "abcdefg" == "abbcddeffg"   &&
    u [1,2,3,4] == [1,2,2,3,4,4]  &&
    u ""        == ""

-- Question 3

data  Proposition  =   Var String
                   |   F
                   |   T
                   |   Not Proposition
                   |   Proposition :|: Proposition
                   |   Proposition :&: Proposition
                   deriving (Eq, Ord, Show)

instance Arbitrary Proposition where
  arbitrary = sized expr
    where
      expr 0 =
        oneof [return F,
               return T,
               liftM Var (elements ["p", "q", "r", "s", "t"])]
      expr n | n > 0 =
        oneof [return F,
               return T,
               liftM Var (elements ["p", "q", "r", "s", "t"]),
               liftM Not (expr (n-1)),
               liftM2 (:&:) (expr (n `div` 2)) (expr (n `div` 2)),
               liftM2 (:|:) (expr (n `div` 2)) (expr (n `div` 2))]

-- 3a.

-- SEE SOLUTIONS

isNorm :: Proposition -> Bool
isNorm (Var p) = True
isNorm F = True
isNorm T = True
--isNorm (Not prop) = False
isNorm (Not (Var p)) = True
--isNorm (p :|: q) = (isNorm p) and (isNorm q)
--isNorm (p :&: q) = (isNorm p) and (isNorm q)

-- 3b.    | (Not prop) == Not (Var prop) = True

norm :: Proposition -> Proposition
norm (Var p) = Var p
norm (Not (Var p)) = Not (Var p)
norm (Not (Not (Var p))) = Var p
norm T = T
norm F = F
norm (p :|: q) = (norm p) :|: (norm q)
norm (p :&: q) = (norm p) :&: (norm q)
norm (Not F) = T
norm (Not T) = F
norm (Not (Not p)) = norm p
norm (Not (p :|: q)) = Not (norm p) :&: Not (norm q)
norm (Not (p :&: q)) = Not (norm p) :|: Not (norm q)
