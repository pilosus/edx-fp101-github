------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

-- data RoseTree a = RoseTree a [RoseTree a] deriving (Show)
data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a
root (r :> []) = r
root (r :> (x:xs)) = r

children :: Rose a -> [Rose a]
children (r :> []) = []
children (r :> (x:xs)) = (x:xs)

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex0 = length $ children ('x' :> map (flip (:>) []) ['a'..'x'])
ex1 = length (children ('x' :> map (\c -> c :> []) ['a'..'A']))
ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================
rt0 = 'a' :> [] -- 1 leaf ['a']
rt1 = 1 :> [2 :> [], 3 :> []] -- 1 + 2 nodes [1], 2 leaves [2, 3]
rt2 = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]
-- 8 + 6 nodes [0, 1, 2, 3, 7, 8, 9, 12], 6 leaves [4, 5, 6, 10, 11, 13]
rtex = 1 :> [1 :> [],2 :> [],3 :> [],4 :> [],5 :> []]
-- 1 + 5 nodes [1], 5 leaves [1, 2, 3, 4, 5]

-- length of list of rose trees
lorlen :: [Rose a] -> (Rose a -> Int) -> Int
lorlen [] f = 0
lorlen (n:ns) f = f n + lorlen ns f

-- produce number of all  nodes of a fiven  Rose Tree (including leaves)
size :: Rose a -> Int
size (r :> []) = 1
size (r :> (x:xs)) = 1 + lorlen (x:xs) size

-- tree = 1 :> map (\c -> c :> []) [1..5]
-- size tree
-- expected & actual: 6

-- tree = 1 :> map (\c -> c :> []) [1..5]
-- size . head . children $ tree
-- expected & actual 1

-- produce number of nodes without children (leaves)
leaves :: Rose a -> Int
leaves (r :> []) = 1
leaves (r :> (x:xs)) = lorlen (x:xs) leaves

-- tree = 1 :> map (\c -> c :> []) [1..5]
-- leaves tree
-- expected & actual: 5

ex3 = size (1 :> map (\c -> c :> []) [1..5])
ex4 = size . head . children $ 1 :> map (\c -> c :> []) [1..5]
ex5 = leaves $ 1 :> map (\c -> c :> []) [1..5]
ex6 = product (map leaves (children $ 1 :> map (\c -> c :> []) [1..5]))
ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
--  fmap :: (a -> b) -> f a -> f b
  fmap f (r :> []) = f r :> []
  fmap f (r :> (x:xs)) = f r :> map (fmap f) (x:xs)

-- fmap (*2) (1 :> [2 :> [], 3 :> []]) == (2 :> [4 :> [], 6 :> []])
-- fmap (+1) (1 :> []) == (2 :> [])

ex8 = size (fmap leaves (fmap (:> []) (1 :> map (\c -> c :> []) [1..5])))
-- ex. 9
-- f r = fmap head $ fmap (\x -> [x]) r
-- :t
-- Rose a -> Rose a
ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

-- https://en.wikibooks.org/wiki/Haskell/Monoids
-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m -- added by vrs

newtype Sum a = Sum a
newtype Product a = Product a

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  Sum x `mappend` Sum y = Sum (x + y)
  mconcat = foldr mappend mempty -- added by vrs, won't work for Rose Tree
  
instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y  = Product (x * y)
  mconcat = foldr mappend mempty -- added by vrs, won't work for Rose Tree

unSum :: Sum a -> a
unSum (Sum a) = a  -- error "you have to implement unSum"
unProduct :: Product a -> a
unProduct (Product a) = a -- error "you have to implement unProduct"

ex11 = unProduct (Product 6 `mappend` (Product . unSum $ Sum 3 `mappend` Sum 4))
-- ex 12
-- :t Sum 3 `mappend` Sum 4
-- Num string => Sum string

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))
num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================
-- https://en.wikibooks.org/wiki/Haskell/Foldable
-- https://www.fpcomplete.com/user/mgsloan/monoids-tour
-- https://stackoverflow.com/questions/26287157/writing-a-foldmap-in-haskell
-- https://stackoverflow.com/questions/26257369/haskell-monoid-foldable-rose-tree

{-

If f is some container-like data structure storing elements of type m
that form a Monoid, then there is a way of folding all the elements in
the data structure into a single element of the monoid m. The
following declaration defines the type class Foldable

It might be the case that we have a foldable data structure storing
elements of type a that do not yet form a Monoid, but where we do have
a function of type Monoid m => a -> m that transforms them into
one. To this end it would be convenient to have a function foldMap ::
Monoid m => (a -> m) -> f a -> m that first transforms all the
elements of the foldable into a Monoid and then folds them into a
single monoidal value.

Add a default implementation of foldMap to the Foldable type class,
expressed in terms of fold and fmap.

-}

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m  -- generalised mconcat
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap g a = fold $ fmap g a

--instance Foldable [] where
--    fold = foldr (mappend) mempty
  
instance Foldable Rose where
  fold (r :> []) = r `mappend` mempty
  fold (r :> (x:xs)) =  r `mappend` (foldr (mappend) mempty (map fold (x:xs)))
  
sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex14 = unProduct $ fold $ fmap Product (1 :> [2 :> [], 3 :> [4 :> []]])
ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================
ex16 = unSum $ foldMap Sum $ 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum r = unSum $ foldMap Sum r
fproduct r = unProduct $ foldMap Product r

ex19 = fsum xs
ex20 = fproduct xs
ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)

