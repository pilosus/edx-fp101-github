double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns
-- $ ghci script.hs
-- :reload

-- convention: function name starts with the lowercase
-- parameter names that ends with s is for a list, with ss - for a list of list
-- xs - name for list of type X, ns - for list of type N

a = b + c
  where
    b = 1
    c = 2
d = a * 2

-- :?  show all commands
-- :quit  exit from GHCi
-- :type expr  show type of an expression
-- :edit  edit current script
-- :edit name  edit script specified
-- :reload  reload current script
-- :load load script specified

-- 6 * 11 - 2
-- (-) ((*) 6 11) 2

-- 6 * (11 - 2)
-- (*) 6 ((-) 11 2)

-- not True && False
-- not (True && False)

-- True || True && False
-- (True || True) && False
-- "Hello" ++ " " ++ "world"
-- length "hello"
-- take 3 "hello"
-- head "hello"
-- last "hello"
-- init "hello"
-- tail "hello"
-- reverse "hello"
-- null "hello"

-- :type True
-- :t "Hello"

-- ['H', 'e', 'l', 'l', 'o']
-- :t ['H', 'e', 'l', 'l', 'o']
-- :t 'H'
-- :t head "Hello"

-- functions also have types
-- :t not

-- :t length
-- Foldable t => t a -> Int

-- list of functions
-- [length, head]

-- Polymorphic functions (can take in different types (a)
-- :t head
-- head :: [a] -> a

-- all elements of the list should be of one type
-- (head [length]) "Hello"


-- tuples
-- Tuples have a fixed length, but elements of different types can be combined. 
-- (1, "Hello")
-- (1, (2, 3))
-- :t ((1, 2), 3)
-- :t (1, 2, 3)
-- :t [1, 2, 3]

-- pairs
-- fst (1, "Hello")
-- snd(1, "Hello")
-- fst (snd (1, (2, 3)))
