module Lab4 where

------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

import Data.Char

-- ===================================
-- Ex. 0
-- ===================================

triangle :: Integer -> Integer
triangle 0 = 0
triangle n = n + (triangle (n - 1))

-- ===================================
-- Ex. 1
-- ===================================

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x xs = if x == head xs then 1 + count x rest else count x rest
  where rest = tail xs

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]

-- count 722 ys
-- count 101 (poem >>= \x -> map (ord . \x -> chr (ord x + 4)) x)

-- ===================================
-- Ex. 2
-- ===================================

euclid :: (Int,  Int) -> Int
euclid (x, y) = if x == y then x else euclid(larger - smaller, smaller)
  where smaller = min x y
        larger = max x y

-- euclid (13404, 8832)

-- ===================================
-- Ex. 3
-- ===================================

funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap _ _ [] = []
funkyMap f g [x] = f x : funkyMap f g []
funkyMap f g (x:y:xs) = (f x) : (g y) : funkyMap f g xs
  -- funkyMap f g xs = 

-- sum $ funkyMap (+10) (+100) ys

-- Ex. 8
-- :t \ a -> a
-- i.e. a -> a

-- Ex. 9
-- :t [undefined]
-- i.e. [a]

-- Ex. 10
-- :t (True, (False))
-- i.e. (Bool, (Bool))

-- Ex. 11
-- :t f a = \b -> (b, a)
-- i.e. t42 -> t4711 -> (t4711, t42)

-- Ex. 12
-- :t foldr id
-- i.e. b -> [b -> b] -> b
-- (a -> ([(a -> a)] -> a))

-- Ex. 13
-- :t flip foldr const
--  (a -> (c -> (b -> c)) -> c -> (b -> c)) -> [a] -> c -> (b -> c)

-- Ex. 14
dup a = (a, a)
-- :t dup . dup . dup :: a -> (((a, a), (a, a)), ((a, a), (a, a)))
-- (a) -> ((((a), (a)), ((a), (a))), (((a), (a)), ((a), (a))))

-- Ex. 15
h g f = (f . g) $ f
-- :t h
-- ((a -> b) -> a) -> ((a -> b) -> b)

-- Ex. 16
fix = h fix
-- :t fix
-- (a -> a) -> a

-- Ex. 17
-- https://wiki.haskell.org/Polymorphism
-- Recursive
-- Polymorphic - uses a instead of a concrete class
-- Higher-order function
-- NOT overloaded (there are no constrained type variables to the left of type like Eq a, or similar)

-- Ex. 18
f = \f n -> if (n == 0) then 1 else n * f (n - 1)
-- :t f
-- f :: (Integer -> Integer) -> Integer -> Integer
-- (Eq a, Num a) => (a -> a) -> a -> a

-- Ex. 19
k = fix $ f
-- k 42
-- (k 42) == 1405006117752879898543142606244511569936384000000000
-- True

-- Ex. 20
-- :t k
-- Polymorphic (uses type a)
-- Overloaded (function f, that k uses, is overloaded - has constrained type (Eq a, Num a))

