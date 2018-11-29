-- Informatics 1 Functional Programming
-- December 2015
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck,
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>), Property )
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char

-- Question 1

-- 1a

p :: [Int] -> Int
p xs = (sum [ x | x <- xs, x > 0] `div` 60) `mod` 12 + 1

-- 1b

q :: [Int] -> Int
q (x:xs) = (d xs `div` 60) `mod` 12 + 1
  where
    d :: [Int] -> Int
    d [] = 1
    d (x:xs)
        | x >= 0 = x + d xs
        | otherwise = d xs

-- 1c

r :: [Int] -> Int
r xs = (duration `div` 60) `mod` 12 + 1
  where
    duration = foldr (+) 0 (filter (>=0) xs)

-- Question 2

-- 2a

f :: String -> String
f "" = ""
f (x:xs) = x:[ y | (x,y) <- zip (x:xs) xs, x /= y]

-- 2b

g :: String -> String
g "" = ""
g [x] = [x]
g (x:y:xs)
    | x == y = g (x:xs)
    | otherwise = x : g (y:xs)

-- Question 3

data Regexp = Epsilon
            | Lit Char
            | Seq Regexp Regexp
            | Or Regexp Regexp
        deriving (Eq, Ord)

-- turns a Regexp into a string approximating normal regular expression notation

showRegexp :: Regexp -> String
showRegexp Epsilon = "e"
showRegexp (Lit c) = [toUpper c]
showRegexp (Seq r1 r2) = "(" ++ showRegexp r1 ++ showRegexp r2 ++ ")"
showRegexp (Or r1 r2) = "(" ++ showRegexp r1 ++ "|" ++ showRegexp r2 ++ ")"

-- for checking equality of languages

equal :: Ord a => [a] -> [a] -> Bool
equal xs ys = sort xs == sort ys

-- For QuickCheck

instance Show Regexp where
    show  =  showRegexp

instance Arbitrary Regexp where
  arbitrary = sized expr
    where
      expr n | n <= 0 = oneof [elements [Epsilon]]
             | otherwise = oneof [ liftM Lit arbitrary
                                 , liftM2 Seq subform subform
                                 , liftM2 Or subform subform
                                 ]
             where
               subform = expr (n `div` 2)



r1 = Seq (Lit 'A') (Or (Lit 'A') (Lit 'A'))   -- A(A|A)
r2 = Seq (Or (Lit 'A') Epsilon)
         (Or (Lit 'A') (Lit 'B'))             -- (A|e)(A|B)
r3 = Seq (Or (Lit 'A') (Seq Epsilon
                            (Lit 'A')))
         (Or (Lit 'A') (Lit 'B'))             -- (A|(eA))(A|B)
r4 = Seq (Or (Lit 'A')
             (Seq Epsilon (Lit 'A')))
         (Seq (Or (Lit 'A') (Lit 'B'))
              Epsilon)                        -- (A|(eA))((A|B)e)
r5 = Seq (Seq (Or (Lit 'A')
                  (Seq Epsilon (Lit 'A')))
              (Or Epsilon (Lit 'B')))
         (Seq (Or (Lit 'A') (Lit 'B'))
              Epsilon)                        -- ((A|(eA))(e|B))((A|B)e)
r6 = Seq (Seq Epsilon Epsilon)
         (Or Epsilon Epsilon)                 -- (ee)(e|e)

-- 3a

language :: Regexp -> [String]
language Epsilon = [""]
language (Lit c) = [[c]]
language (Seq r1 r2) = nub [s1++s2 | s1 <- language r1, s2 <- language r2]
language (Or r1 r2) = nub (language r1 ++ language r2)

-- 3b

simplify :: Regexp -> Regexp
simplify (Seq Epsilon r2)= r2
simplify (Seq r1 Epsilon) = r1
simplify () 
