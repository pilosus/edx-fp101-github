-- exercise 0
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

halve' xs = splitAt (length xs `div` 2) xs
halve'' xs = splitAt (div (length xs) 2) xs
halve''' xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

-- exercise 1
safetail :: [a] -> [a]

safetail xs = if null xs then [] else tail xs

safetail' [] = []
safetail' (_:xs) = xs

safetail'' xs | null xs = []
              | otherwise = tail xs

safetail''' [] = []
safetail''' xs = tail xs

safetail'''' = \ xs -> case xs of
  [] -> []
  (_:xs) -> xs

-- exercise 2
-- import Prelude hiding ((||))


or' False False = False
or' _  _ = True

or'' b c | b == c = b
         | otherwise = True

or''' b False = b
or''' _ True = True


or'''' b c | b == c = c
           | otherwise = True

or5 False False = False
or5 False True = True
or5 True False = True
or5 True True = True

-- exercise 3
-- import Prelude hiding ((&&))
and' True True = True
and' _ _ = False

and'' a b = if a then if b then True else False else False
and''' a b = if a then b else False
and'''' a b = if b then a else False 

-- exercise 4
mult = \x -> (\y -> (\z -> x * y * z))

-- exercise 5
-- f x g y
-- ((f x) g) y

-- exercise 6
-- f :: (a -> a) -> a
-- Takes a function as its argument

-- exercise 7
remove :: Int -> [a] -> [a]
remove n xs = take n xs  ++ drop (n + 1) xs

-- exercise 8
funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs

