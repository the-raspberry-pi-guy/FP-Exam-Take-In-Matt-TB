-- Informatics 1 Functional Programming
-- Mock exam
--
-- Solutions


import Data.Char
import Data.List
import Control.Monad
import Test.QuickCheck

-- Question 1
-- 1a.

isFaceCard :: Char -> Bool
isFaceCard x = x == 'A' || x == 'K' || x == 'Q' || x == 'J'

isCard :: Char -> Bool
isCard x = isFaceCard x || (isDigit x && x /= '1')

f :: String -> Bool
f str = and [ isFaceCard c | c <- str, isCard c ]

test1a =
    f "ABCDE"     == True   &&
    f "none here" == True   &&
    f "4 Aces"    == False  &&
    f "01234"     == False  &&
    f ""          == True   &&
    f "1 Ace"     == True   




-- 1b.

g :: String -> Bool
g [] = True
g (c:str) | isCard c  = isFaceCard c && g str
          | otherwise = g str


test1b =
    g "ABCDE"     == True   &&
    g "none here" == True   &&
    g "4 Aces"    == False  &&
    g "01234"     == False  &&
    g ""          == True   &&
    g "1 Ace"     == True   



-- 1c.

h :: String -> Bool
h str = foldr (&&) True (map isFaceCard (filter isCard str))

test1c =
    h "ABCDE"     == True   &&
    h "none here" == True   &&
    h "4 Aces"    == False  &&
    h "01234"     == False  &&
    h ""          == True   &&
    h "1 Ace"     == True   

test1 = test1a && test1b && test1c
        
prop_fgh :: String -> Bool
prop_fgh str = f str == g str && f str == h str


-- Question 2
-- 2a.

t :: [a] -> [a]
t xs = concat [ if odd i then [x] else [x,x] | (x,i) <- zip xs [1..] ]

test2a =
    t "abcdefg" == "abbcddeffg"   &&
    t [1,2,3,4] == [1,2,2,3,4,4]  &&
    t ""        == ""


-- 2b.

u :: [a] -> [a]
u [] = []
u [x] = [x]
u (x:y:xs) = x : y : y : u xs


test2b =
    u "abcdefg" == "abbcddeffg"   &&
    u [1,2,3,4] == [1,2,2,3,4,4]  &&
    u ""        == ""

test2 = test2a && test2b
        
prop_tu_string :: String -> Bool
prop_tu_string str = t str == u str

prop_tu_intlist :: [Int] -> Bool
prop_tu_intlist xs = t xs == u xs


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

isNorm :: Proposition -> Bool
isNorm (Var x)           =  True
isNorm T                 =  True
isNorm F                 =  True
isNorm (Not (Var x))     =  True
isNorm (Not p)           =  False
isNorm (p :|: q)         =  isNorm p && isNorm q
isNorm (p :&: q)         =  isNorm p && isNorm q

test3a =
  isNorm (Var "p" :&: Not (Var "q"))        ==  True   &&
  isNorm (Not (Var "p" :|: Var "q"))        ==  False  &&
  isNorm (Not (Not (Var "p")) :|: Not T)    ==  False  &&
  isNorm (Not (Var "p" :&: Not (Var "q")))  ==  False

-- 3b.

norm :: Proposition -> Proposition
norm (Var x)          =  Var x
norm T                =  T
norm F                =  F
norm (Not (Var x))    =  Not (Var x)
norm (Not T)          =  F
norm (Not F)          =  T
norm (Not (Not p))    =  norm p
norm (Not (p :|: q))  =  norm (Not p) :&: norm (Not q)
norm (Not (p :&: q))  =  norm (Not p) :|: norm (Not q)
norm (p :|: q)        =  norm p :|: norm q
norm (p :&: q)        =  norm p :&: norm q

test3b =
     norm (Var "p" :&: Not (Var "q"))
      ==  (Var "p" :&: Not (Var "q"))
  && norm (Not (Var "p" :|: Var "q"))
      ==  Not (Var "p") :&: Not (Var "q")
  && norm (Not (Not (Var "p")) :|: Not T)
      ==  (Var "p" :|: F)  
  && norm (Not (Var "p" :&: Not (Var "q")))
      ==  Not (Var "p") :|: Var "q"

test3 = test3a && test3b

prop3 :: Proposition -> Bool
prop3 p  =  isNorm (norm p)
