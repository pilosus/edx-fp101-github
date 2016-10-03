import Prelude hiding (Left, Right, Up, Down, Maybe, Nothing, Just)

-- helper functions
type Bit = Int
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- remove duplicates
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)


--
-- Type declarations
--

-- defining synonymous

type String1 = [Char]
type Board = [Pos]
type Pos = (Int, Int)

-- type declarations caanot be recursive!
-- erroneous declaration!
-- type Tree = (Int [Tree])

-- recursive types can be declared using data mechanism

-- parameterisied types
type Parser a = String -> [(a, String)]

-- associate key value
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

--
-- Data declarations
--

-- read "|" as "or"
data NewBool = False | True

data Move = Left | Right | Up | Down
move :: Move -> Pos -> Pos
move Left (x, y) = (x - 1, y)
move Right (x, y) = (x + 1, y)
move Up (x, y) = (x, y - 1)
move Down (x, y) = (x, y + 1)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

flip :: Move -> Move
flip Left = Right
flip Right = Left
flip Up = Down
flip Down = Up

-- data declaration can also have arguments
-- Circle 7.0 is a Shape
-- Rect 10.5 5.5 is a Shape
data Shape = Circle Float | Rect Float Float

-- produce a square of a given size
square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y
-- area (Rect 10.0 20.0)

{-

The difference between normal functions and constructor functions is
that the latter cannot be further evaluated of simplified. It's
already fully evaluated.

-}

-- parameterised data declarations

data Maybe a = Nothing | Just a
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- Homework ex 9
-- Maybe as a Monad
-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/Maybe
instance Monad Maybe where
  return x = Just x
  Nothing >>= _ = Nothing
  (Just x) >>= f = f x

-- Homework ex 10
-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/List
{-
instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
-}

-- Homework ex 11
-- https://wiki.haskell.org/Monoid
class Monoid m where
    mempty :: m
    --mappend :: m -> m -> m
    (<>) :: m -> m -> m
    mconcat :: [m] -> m
    -- defining mconcat is optional, since it has the following default:
    --mconcat = foldr mappend mempty
    mconcat = foldr (<>) mempty

instance Monoid [a] where
    mempty = []
    (<>) = (++) -- mappend

-- Homework ex 12
-- A functor is a type constructor with an operation
-- fmap :: Functor f => (a -> b) -> f a -> f b such that (fmap f)
-- . (fmap g) = fmap (f . g) and fmap id = id, ignoring the existence
-- of bottoms.

-- https://wiki.haskell.org/Functor
class Functor1 f where
    fmap :: (a -> b) -> f a -> f b

-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids
instance Functor1 Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing  


-- Homework ex 13
class (Functor f) => Foldable1 f where
  fold' :: (Monoid m) => f m -> m

instance Foldable1 [] where
  fold' = foldr (<>) mempty


--  The module Data.Foldable defines the following type class for
--  folding functors with monoid elements:

class (Functor f) => Foldable f where
  fold :: (Monoid m) => f m -> m

-- fold :: (Foldable f, Monoid a) => f a -> a
instance Foldable [] where
  

--
-- Recursive types
--

data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
--add m n = int2nat (nat2int m + nat2int n)
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult m (Succ n) = add m (mult m n)

-- list data declaration
data List a = Nil | Cons a (List a)
len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

--
data Ordering = LT | EQ | GT
-- compare :: (Ord a) => a -> a -> Ordering

-- binary trees
data Tree = Leaf Int | Node Tree Int Tree deriving (Show)
t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
--occurs m (Node l n r) = m == n || occurs m l || occurs m r
occurs m (Node l n r)
  | m == n = Prelude.True
  | m < n = occurs m l
  | otherwise = occurs m r


flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

-- tree that has data only in its leaves
data Tree1 a = Leaf1 a | Node1 (Tree1 a) (Tree1 a)

-- tree that has data only in its nodes
data Tree2 a = Leaf2 | Node2 (Tree2 a) a (Tree2 a)

-- tree that has data in both its leaves and nodes
data Tree3 a b = Leaf3 a | Node3 (Tree3 a b) b (Tree3 a b)

-- tree that has a list of subtrees
data Tree4 a = Node4 a [Tree4 a]

-- homework ex 5
data TreeB  = LeafB Int | NodeB TreeB TreeB deriving (Show)

-- tree is balanced if the number of leaves in the left and right
-- subtree of every node differs by at most one, with leaves
-- themselves being trivially balanced.

-- produce True if a tree is balanced, False otherwise
leaves (LeafB _) = 1
leaves (NodeB l r) = leaves l + leaves r
balanced :: TreeB -> Bool
balanced (LeafB _ ) = Prelude.True
balanced (NodeB l r) =
  abs (leaves l - leaves r) <= 1 && balanced l && balanced r


-- homework ex 6
-- define function that converts a finite, non-empty, non-partial,
-- non-bottom list of non-bottom integers into a balanced tree.

halve xs = splitAt (length xs `div` 2) xs
balance :: [Int] -> TreeB
balance [x] = LeafB x
balance xs = NodeB (balance ys) (balance zs)
  where (ys, zs) = halve xs

-- Homework ex 7
-- Add (Val 1) (Val 2) is a value of the datatype:
-- data Expr = Add Expr Expr | Val Int

-- Homework ex 8
-- Node (Leaf 1) (Leaf 2) is a value of the datatype:
-- data Tree = Leaf Int | Node Tree Tree


--
-- Tautologies checker
--
-- tautology is a proposition that is always True

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          deriving (Show)

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool
-- substitution [ (’A’, False ), (’B’, True ) ] assigns the variable A
-- to False, and B to True.

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
-- imply is implemented by <= on logical values

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (Prelude.False:) bss ++ map (Prelude.True:) bss
  where bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

-- substs p2
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
-- isTaut p4

{-
bools n = map (map conv . make n . int2bin) [0..limit]
  where
    limit = (2 ^ n) - 1
    make n bs = take n (bs ++ repeat 0)
    conv 0 = Prelude.False
    conv 1 = Prelude.True

-}
