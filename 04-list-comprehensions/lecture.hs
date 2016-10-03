-- List comprehension

import Data.Char -- isLower

-- [x^2 | x <- [1..5]]
-- [(x, y) | x <- [1..3], y <- ['a'..'e']]
-- [(x, y) | x <- [1..3], y <- [x..3]]

nconcat :: [[a]] -> [a]
nconcat xss = [x | xs <- xss, x <- xs]

firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps ]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs] -- substitute each element with 1, then sum up

-- Guards
-- [x | x <- [1..10], even x]
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- zip ['a', 'b', 'c'] [1..4]
adjpairs :: [a] -> [(a, a)]
adjpairs xs = zip xs (tail xs)

-- produce True if elements of the list ordered in ascending order
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- adjpairs xs]

-- produce positions in the given list where given value is found
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                 where n = length xs - 1

-- produce number of lower case letters in the given string
lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

uppers :: String -> Int
uppers xs = length [x | x <- xs, isUpper x]

-- produce number of appearances of the given character in the given string
count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- Caesar cipher

-- produce position of the lowercase chracter between 0 and 25
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- produce position of the uppercase chracter between 0 and 2
uplet2int :: Char -> Int
uplet2int c = ord c - ord 'A'


-- produce a lowercase charater by its position number
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- produce a uppercase charater by its position number
int2uplet :: Int -> Char
int2uplet n = chr (ord 'A' + n)

-- produce position shiften by n and wrapped around at 26
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2uplet ((uplet2int c + n) `mod` 26) -- added for exercise 10
          | otherwise = c

-- produce Ceasar's code of the given string by shifting each char by n
encode :: Int -> [Char] -> [Char]
encode n xs = [shift n x | x <- xs]

-- table of letters occurence frequencies in English texts
table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
        6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- produce percentage of n in m
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- produce a frquency table for the given string
freqs :: String -> [Float]
freqs xs = [percent (count x xs) l | x <- ['a'..'z']]
  where l = lowers xs + uppers xs
           --where l = lowers xs

-- comparing result with frequency table for English texts
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

-- produce list by wrapping the given list around the given position
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

--
crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs

--
-- Exercises
--

-- 1) 338350
-- sum [x^2 | x <- [1..100]]

-- 2)
-- 

replicate' :: Int -> a -> [a]
replicate' n e = [e | _ <- [1..n]]

-- 3)
--
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- :set +s in ghci for benchmark

-- 4) Positive int is perfect if it equals the sum of its factors, excluding the number itself
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect num = sum (init (factors num)) == num
--perfects n = [x | x <- [1..n], sum (init (factors x)) == x]


-- 5) rewrite [(x,y) | x <- [1,2,3], y <- [4,5,6]]
-- with one generator instead two, using concat and nest one comprehension within the other
-- concat [[(x, y) | y <- [4,5,6]] | x <- [1,2,3]]

-- 6) redifine positions with find
positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x [y | y <- zip xs [0..(length xs - 1)]]
  
-- positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
--                 where n = length xs - 1

-- find :: Eq a => a -> [(a, b)] -> [b]
-- find k t = [v | (k', v) <- t, k == k']


-- 7)
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y)<- xs `zip` ys]
--scalarproduct xs ys = sum [x * y | (x, y)<- zip xs ys]

-- 8) Caesar cipher with uppercase letter added above

-- 12) takes two lists of the same length and interleaves their elements in turn about order. 
--  riffle [1,2,3] [4,5,6] = [1, 4, 2, 5, 3, 6]
riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]

-- 13)

divides :: Int -> Int -> Bool
divides a b = a `mod` b == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], x `divides` d]
