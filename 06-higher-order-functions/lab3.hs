module Lab3 where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens xs = [x | x <- xs, even x]

-- ===================================
-- Ex. 3 - 4 
-- ===================================

-- complete the following line with the correct type signature for this function
squares :: Integer -> [Integer] 
squares n = [x ^ 2 | x <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
squares' :: Int -> Int -> [Int]
squares' m n = take m [x ^ 2 | x <- [(n + 1)..]]

sumSquares' :: Int -> Int
sumSquares' x = sum . uncurry squares' $ (x, x)

-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- added by vrs

-- ===================================
-- Ex. 9
-- ===================================
-- a f b g c
-- is equivalent to:
-- ((a f) b) g c 

-- ===================================
-- Ex. 10
-- ===================================
-- The type a -> f -> b -> g -> c is equivalent to:
-- a -> (f -> b -> (g -> c))

-- ===================================
-- Ex. 11
-- ===================================
-- The type (a, f, b, g, c) is equivalent to:
-- ((a, f, b, g, c))

-- ===================================
-- Ex. 12
-- ===================================
-- The expression (a, f, b, g, c) is equivalent to:
-- ((a, f, b, g, c)) 
