-- exercise 1
n = a `div` length xs
  where a = 10
        xs = [1, 2, 3, 4, 5]

-- exercise 2
last1 xs = head (drop (length xs - 1) xs)
last2 xs = head (reverse xs)
last3 xs = xs !! (length xs - 1)

-- exercise 3
init1 xs = reverse (tail (reverse xs))

-- exercise 5
sum1 [] = 0
sum1 (x : xs) = x + sum1 xs

-- sum [x]
-- =   { applying sum }
-- x + sum []
-- =   { applying sum }
-- x + 0
-- =   { applying + }
-- x

-- exercise 6
product1 [] = 1
product1 (x : xs) = x * product1 xs

-- exercise 7
rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
                where
                  smaller = [a | a <- xs, a <= x]
                  larger =  [b | b <- xs, b > x]

-- 
rqsort' [] = []
rqsort' (x:xs) = reverse (reverse (rqsort' smaller) ++ [x] ++ reverse (rqsort' larger))
                where
                  smaller = [a | a <- xs, a <= x]
                  larger =  [b | b <- xs, b > x]

--
rqsort'' [] = []
rqsort'' (x:xs) = rqsort'' larger ++ [x] ++ rqsort'' smaller
                where
                  smaller = [a | a <- xs, a < x || a == x]
                  larger =  [b | b <- xs, b > x]

-- let l = [1, 6, 7, 0, 4, 5, 7, 8]

-- exercise 8
qsort''' [] = []
qsort''' (x:xs) = qsort''' smaller ++ [x] ++ qsort''' larger
                where
                  smaller = [a | a <- xs, a < x]
                  larger =  [b | b <- xs, b > x]
