module Lecture10 where
import Prelude hiding (Eq, (==), (/=), Ord, (<), (<=), (>), (>=), min, max)

-- https://stackoverflow.com/questions/16430025/ambiguous-occurrence

--
-- Class and instance declarations
--

class Eq a where
      (==) :: a -> a -> Bool
      (/=) :: a -> a -> Bool
      --x == y = not (x /= y)
      x /= y = not (x == y) 

instance Eq Bool where
  False == False = True
  True == True = True
  _ == _ = False

-- declare a new class by extending the existing one
class Eq a => Ord a where
  (<), (<=), (>), (>=) :: a -> a -> Bool
  min, max :: a -> a -> a
  min x y | x <= y = x
          | otherwise = y
  max x y | x <= y = y
          | otherwise = x


instance Ord Bool where
  False < True = True
  _ < _ = False
  b <= c = (b < c) || (b == c)
  b > c = c < b
  b >= c = c <= b

-- Derived instances
-- add new types into built-in classes

-- data Bool = False | True
--           deriving (Eq, Ord, Show, Read)

-- Now all the member functions from the four derived classes can then
-- be used with logical values

-- show True
-- read "False" :: Bool
