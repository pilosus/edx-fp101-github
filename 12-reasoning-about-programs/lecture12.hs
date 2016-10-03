module Lecture11 where
import Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

reverse' :: [a] -> [a] -> [a]
reverse' [] ys = ys
reverse' (x:xs) ys = reverse' xs (x:ys)

reverse1 :: [a] -> [a]
reverse1 xs = reverse' xs []

--reverse2 :: [a] -> [a]
--reverse2 = foldl (:) []

data Tree = Leaf Int | Node Tree Tree

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r

flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n) ns = n:ns
flatten' (Node l r) ns = flatten' l (flatten' r ns)

flatten1 :: Tree -> [Int]
flatten1 t = flatten' t []

--
-- Compiler correctness
--

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD deriving (Show)

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n:c) s = exec c (n:s) -- place a new int on the top of the stack
exec (ADD:c) (m:n:s) = exec c (n + m:s) -- take two values from the
                                        -- stack, add them up, and
                                        -- replace them by the result

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

-- e :: Expr
-- e = Add (Add (Val 2) (Val 3)) (Val 4)
-- eval e
-- comp e

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n:c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

comp1 :: Expr -> Code
comp1 e = comp' e []
