-- Informatics 1 Functional Programming
-- December 2016
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck,
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>), Property )
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char

-- Question 1

-- 1a

f :: [Int] -> [Int] -> Int
f xs ys = sum [x | (x,y) <- zip xs ys, x `mod` y == 0]

-- 1b

g :: [Int] -> [Int] -> Int
g [] [] = 0
g _ [] = 0
g [] _ = 0
g (x:xs) (y:ys)
    | x `mod` y == 0 = x + g xs ys
    | otherwise = g xs ys

-- Question 2

-- 2a

p :: String -> Int
p [] = 0
p xs = maximum (0:[digitToInt x | x <- xs, isDigit x])

-- 2b

q :: String -> Int
q [] = 0
q (x:xs)
     | isDigit x = max (digitToInt x) (q xs)
     | otherwise = q xs

-- 2c

r :: String -> Int
r xs = foldr max 0 (map digitToInt (filter isDigit xs))

-- Question 3

data Move =
     Go Int            -- move the given distance in the current direction
   | Turn              -- reverse direction
   | Dance             -- dance in place, without changing direction
  deriving (Eq,Show)   -- defines obvious == and show

data Command =
     Nil                      -- do nothing
   | Command :#: Move         -- do a command followed by a move
  deriving Eq                 -- defines obvious ==

instance Show Command where   -- defines show :: Command -> String
  show Nil = "Nil"
  show (com :#: mov) = show com ++ " :#: " ++ show mov

type Position = Int
data Direction = L | R
  deriving (Eq,Show)          -- defines obvious == and show
type State = (Position, Direction)

-- For QuickCheck

instance Arbitrary Move where
  arbitrary = sized expr
    where
      expr n | n <= 0 = elements [Turn, Dance]
             | otherwise = liftM (Go) arbitrary

instance Arbitrary Command where
  arbitrary = sized expr
    where
      expr n | n <= 0 = oneof [elements [Nil]]
             | otherwise = oneof [ liftM2 (:#:) subform arbitrary
                                 ]
             where
               subform = expr (n-1)

instance Arbitrary Direction where
  arbitrary = elements [L,R]

-- 3a

state :: Move -> State -> State
state = undefined

-- 3b

trace :: Command -> State -> [State]
trace = undefined

-- 3c

dancify :: Command -> Command
dancify = undefined
