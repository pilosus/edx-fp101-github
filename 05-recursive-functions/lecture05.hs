-- 6. Recursive functions
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

lprod :: Num a => [a] -> a
lprod [] = 1
lprod (n:ns) = n * lprod ns

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
                | otherwise = y:insert x ys

-- insertion sort
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- multiple arguments
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _  = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip xs ys

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (_:xs)  = drop' (n - 1) xs

-- multiple recursions
-- produce nth Fibonacci number
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

-- quicksort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- mutual recursive
even' :: Int -> Bool
even' 0 = True
even' n = odd (n - 1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even (n - 1)

-- produce a list of all element at even positions starting from 0 for the first element
evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x:odds xs
-- produce a list of all element at odd positions starting from 0 for the first element
odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

prod' :: Num a => [a] -> a
prod' = foldr (*) 1

init' :: [a] -> [a]
init' [_] = []
init' (x:xs) =  x:init xs

--
-- Exercises
--

-- 1. Define exponential function recursively

pow :: Int -> Int -> Int
pow _ 0 = 1
pow a b = a * (pow a (b - 1))

-- 2.

-- a. and for a list
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = if x then and' xs else False

-- b. concat
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) =  x ++ concat' xs

-- c. Produce a list with n identical elements

replicate' :: Int -> a -> [a]
replicate' 0 e = []
replicate' n e = e : (replicate' (n - 1) e)

-- d. produce the nth element of a list (analogue of !!)
sel :: Int -> [a] -> a
sel 0 (x:xs) = x
sel n (x:xs) = sel (n - 1) xs

-- e. produce True if a value is an element of a list
elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs)  | x == e = True
                | otherwise = elem' e xs
--elem' e (x:xs) = if x == e then True else elem' e xs

-- 4. ???
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)


-- 5. merge sort algorithm using merge and halve functions
--- !!!
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
           where (ys, zs) = halve xs
--msort (x:xs) = merge (msort (fst halves)) (msort (snd halves))
--  where halves = halve (x:xs)

-- produce a list of pair containing two halves of the given list
-- assume: lengths of the halves may differ by at most one

halve :: [a] -> ([a], [a])
halve xs  = (take n xs, drop n xs)
  where n  = (length xs) `div` 2

--halve :: [a] -> [([a], [a])]
--halve xs  = [(take n xs, drop n xs)]
--  where n  = (length xs) `div` 2

-- 6.
-- a. produce sum of a list numbers

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' :: Num a => [a] -> a
sum'' = foldr (+) 0

-- b. produce list of the first n element of the fiven list

take' :: Int -> [a] -> [a]
take' n [] = []
take' 0 (x:xs) = []
take' n (x:xs) = x : take (n - 1) xs

-- c. produce the last element of the list
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs
