module Chapter7 where
import Data.Char

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- map isDigit ['1', 'a', '2']
-- map reverse ["abc", "cde", "fgh"]

-- map (map (+ 1)) [[1,2,3], [4,5]]
-- [[2,3,4], [5,6]]

map'' :: (a -> b) -> [a] -> [b]
map'' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

-- filter even [1..10]
-- filter (/= ' ') "abc cde efg"
-- filter (> 5) [1..20]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p [] = []
filter'' p (x:xs) | p x = x : filter'' p xs
                  | otherwise = filter'' p xs

-- andmap analogue
--all even [2,4,6,8] -- True
--all odd [1,4,5,8] -- False

-- select elements from a list while they satisfy a predicate
--takeWhile isLower "abcdEFfgh" -- "abcd"

-- remove elements from a list while they satisfy a predicate
--dropWhile isLower "abcdEFfgh" -- "EFfgh"

-- fold function
sum' xs = foldr (+) 0 xs
product' xs = foldr (*) 1 xs
or' xs = foldr (||) False xs
and' xs = foldr (&&) True xs
-- could be defined as follows:
-- and' = foldr (&&) True
-- but ghci does not allow this for some reasons

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr f v xs)

length' :: [a] -> Int
length' xs = foldr (\ _ n -> 1 + n) 0 xs

snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
reverse' xs = foldr snoc [] xs

-- fold left
lenght'' :: [a] -> Int
lenght'' xs = foldl (\ n _ -> n + 1) 0 xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs) = foldl f (f v x) xs
-- v is an accumulator

-- compositions operator '.' - compositions of 2 functions
comp :: (b -> c) -> (a -> b) -> (a -> c)
comp f g = \ x -> f (g x)

odd' = not . even
twice f = f . f
sumsqreven = sum . map (^ 2) . filter even
-- function compositions is associative, i.e. f . (g . h) = (f . g) . h

-- string transmitter example
type Bit = Int

bin2int :: [Bit] -> Int
--bit2int bits = sum [w * b | (w, b) <- zip weights bits]
--               where weights = iterate (*2) 1
bin2int = foldr (\ x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0) -- repeat 0 is infinite list of zeroes

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
