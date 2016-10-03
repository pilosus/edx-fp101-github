module Lecture10 where

data Op = Add | Sub | Mul | Div deriving (Show)

valid :: Op -> Int -> Int -> Bool
-- see the bottom, algebraic properties
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

{-
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0
-}

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr deriving (Show)

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                    y <- eval r,
                    valid o x y]
                   

-- return all sub- sequences of a list, which are given by all
-- possible combinations of excluding or including each element
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

-- subs [1,2,3]

-- return all possible ways of inserting a new element into a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)
-- interleave 1 [2,3,4]

-- return all permutations of a list, which are given by all possible
-- reorderings of the elements:
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))
-- perms [1,2,3]

-- return all choices from a list, which are given by all possible
-- ways of selecting zero or more elements in any order
choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

choices' :: [a] -> [[a]]
choices' xs = [zs | ys <- subs xs, zs <- perms ys]
-- choices [1,2,3]

-- procude true if the problem can be solved
--solution :: Expr -> [Int] -> Int -> Bool
--solution e ns n = elem (values e) (choices ns) && eval e == [n]

--
-- Brute force solution
--

-- return all possible ways of splitting a list into two non-empty
-- lists that append to give the original list
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls,rs) <- split xs]
-- split [1,2,3,4]

-- return all possible expressions whose list of values is precisely a
-- given list
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
            l <- exprs ls,
            r <- exprs rs,
            e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]
ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutionsbf :: [Int] -> Int -> [Expr]
solutionsbf ns n = [e | ns' <- choices ns,
                  e <- exprs ns',
                  eval e == [n]]
                 

--
-- Combining generation and evaluation
--

type Result = (Expr, Int)

-- return all possible results comprising expressions whose list of
-- values is precisely a given list

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
              lx <- results ls,
              ry <- results rs,
              res <- combine' lx ry]


combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

-- return all possible expressions that solve an instance of the
-- countdown problem, by first generating all results over each choice
-- from the given numbers, and then selecting those expressions whose
-- value is the target
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns,
                   (e,m) <- results ns',
                   m == n]
-- solutions' [ 1, 3, 7, 10, 25, 50 ] 765

--
-- Exploiting algebraic properties
--

-- commutativity and identity properties:
-- x + y = y + x
-- x * y = y * x
-- x * 1 = x
-- 1 * y = y
-- x `div` 1 = x

--
-- Homework 10
--

-- Ex 1
-- remove the first occurence of a given element from a list
removeone :: Eq a => a -> [a] -> [a]
removeone x [] = []
removeone x (y:ys)
  | x == y = ys
  | otherwise = y : removeone x ys
-- removeone 5 [1,2,3,4,5,5,5,6,7]

-- Ex 2
-- decides whether one list is chosen from another, i.e. checks
-- whether all elements in xs are present in ys.
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeone x ys)
-- isChoice [1,2,3] [0,9,8,7,1,5,7,8,1,2,6,3,0,3]
-- True

