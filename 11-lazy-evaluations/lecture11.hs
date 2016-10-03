module Lecture11 where

--
-- Evaluation strategies
--
mult :: (Int, Int) -> Int
mult (x, y) = x * y

-- currying using lambda expressions
mult' :: Int -> Int -> Int
mult' x = \ y -> x * y

ones :: [Int]
ones = 1 : ones

-- head ones
-- take 3 ones

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n [] = []
take' n (x:xs) = x : take (n - 1) xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

-- does not terminate!
-- filter (<= 5) [1..]

-- does terminate
-- takeWhile (<= 5) [1..]

--
-- sieve of Eratosthenes
--

primes :: [Int]
primes = sieve [2..]

sieve  :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- take 100 primes
-- takeWhile (< 100) primes

--
-- Strict application
--

-- f $! x
-- top-level of evaluation of the argument expression x is forced
-- before the function f is applied

-- if x is of basic types like Int or Bool, then it's a complete evaluation of argument

-- if it's a pair like (Int, Bool), then evaluation is performed until
-- a pair of expressions is obtained, but no further.

-- f x y
-- (f $! x) y -- forces top-level evaluation of x
-- (f x) $! y -- forces top-level evaluation of y
-- (f $! x) $! y -- forces top-level evaluation of x and y

-- strict application is used to improved the space perfomance

sumwith :: Int -> [Int] -> Int
sumwith v [] = v
sumwith v (x:xs) = sumwith (v + x) xs
-- sumwith 0 [1,2,3] -> ... sumwith (((0 + 1) + 2) + 3) []

-- redifined with strict application
sumwith' :: Int -> [Int] -> Int
sumwith' v [] = v
sumwith' v (x:xs) = (sumwith $! (v + x)) xs
-- sumwith 0 [1,2,3] -> ... sumwith $! 6 []

-- foldl with strict application
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs) = ((foldl' f) $! (f v x)) xs

sumwith'' = foldl' (+)

--
-- Homework
--

-- Ex 6
-- infinite sequence of Fibonacci numbers
fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

-- Ex 7
-- produce n-th Fibonacci number
fib :: Int -> Integer
fib n = fibs !! n

-- Ex 8
-- calculate the first Fibonacci number greater than 1000
largeFib :: Integer
largeFib = head (dropWhile (<= 1000) fibs)

-- Ex 9
repeat' :: a -> [a]
repeat' x = xs
  where xs = x : xs

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
              deriving (Show)

repeatTree :: a -> Tree a
repeatTree x = Node t x t
  where t = repeatTree x
