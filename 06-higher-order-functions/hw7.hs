module Homework6 where
import Data.Char
-- Exercise 0
-- [f x | x <- xs, p x] == map f (filter p xs)

-- Exercise 1

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and (map p xs)

all3 :: (a -> Bool) -> [a] -> Bool
all3 p = and . map p

all4 :: (a -> Bool) -> [a] -> Bool
all4 p = not . any (not . p)

all5 :: (a -> Bool) -> [a] -> Bool
all5 p xs = foldl (&&) True (map p xs)

all6 :: (a -> Bool) -> [a] -> Bool
all6 p = foldr (&&) True . map p

-- Exercise 2

any2 :: (a -> Bool) -> [a] -> Bool
any2 p = or . map p

any3 :: (a -> Bool) -> [a] -> Bool
any3 p xs = length (filter p xs) > 0

any4 :: (a -> Bool) -> [a] -> Bool
any4 p = not . null . dropWhile (not . p)

any5 :: (a -> Bool) -> [a] -> Bool
any5 p xs = not (all (\ x -> not (p x)) xs)

any6 :: (a -> Bool) -> [a] -> Bool
any6 p xs = foldr (\ x acc -> (p x) || acc) False xs

-- Exercise 3

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x:xs) | p x = x : takeWhile p xs
                    | otherwise = []

-- Exercise 4

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x:xs) | p x = dropWhile p xs
                    | otherwise = (x:xs)

-- Exercise 5

map1 :: (a -> b) -> [a] -> [b]
map1 f = foldl (\ xs x -> xs ++ [f x]) []
-- map1 f = foldr (\ x xs -> f x : xs) []

-- Exercise 6

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p = foldr (\ x xs -> if p x then x:xs else xs) []

-- Exercise 7

dec2int :: [Integer] -> Integer
dec2int = foldl (\ x y -> 10 * x + y) 0

-- Exercise 8

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- sumsqreven = compose [sum, map (^ 2), filter even] -- it does not typecheck!
-- is: sum [map (^ 2) (filter even [1..10])]

-- Exercise 9

--curry1 :: ((a -> b) -> c) -> a -> b -> c
curry1 f = \ x y -> f (x, y)

-- Exercise 10
--uncurry1 :: (a -> b -> c) -> (a -> b) -> c
uncurry1 f = \ (x, y) -> f x y

-- Exercise 11

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- NB
int2bin2 :: Int -> [Bit]
int2bin2 = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- NB
chop82 :: [Bit] -> [[Bit]]
chop82 = unfold null (take 8) (drop 8)

-- Exercise 12

mapuf :: (a -> b) -> [a] -> [b]
mapuf f = unfold null (f . head) tail

-- Exercise 13
iterateuf :: (a -> a) -> a -> [a]
iterateuf f = unfold (const False) id f

-- take 10 (iterate (*2) 1) == [1, 2, 4, 8, 16, 32, 64, 128, 256, 512]

-- Exercise 14
-- f . (g . h) = (f . g) . h

-- Exercise 15
-- is not valid: [x] : xs = [x, xs]

-- Exercise 16
-- filter p . filter p = filter p

-- Exercise 17
-- reverse (map f xs) = map f (reverse xs)

-- Exercise 18
-- reverse (xs ++ ys) = reverse ys ++ reverse xs

-- Exercise 19
-- take 10 [1..] is finite

-- Exercise 20
-- sum is a higher-order function -- is false statement

-- Exercise 21
-- map is a function with two arguments  -- is wrong

-- Exercise 22
-- foldr is an overloaded function -- is wrong statement

-- Exercise 23
-- take is a polymorphic function -- is a true statement

-- Exercise 24
-- f x = x > 3  -- f is overloaded

-- Exercise 25
-- take 4 (iterate (+1) 1)  == [1, 2, 3, 4]

-- Exercise 26
-- takeWhile even [2, 4, 5, 6, 7, 8] == [2,4]

-- Exercise 27
-- zip [1, 2] ['a', 'b', 'c'] == [(1,'a'),(2,'b')]

-- Exercise 28
-- foldr (-) 0 [1, 2, 3, 4] == -2

-- Exercise 29
-- filter even (map (+1) [1..5]) == [2,4,6]

-- Exercise 30
-- [f x|x <- xs, p (f x)] == filter p (map f xs)

-- Exercise 31
-- cExp :: CNat -> CNat -> CNat
-- cExp (CNat a) (CNat b) = CNat (b a)
