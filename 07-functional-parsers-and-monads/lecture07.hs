type Parser a = String -> [(a, String)]
--
-- Basic parsers
--

-- Parser that always succeeds
return' :: a -> Parser a
return' v = \ inp -> [(v, inp)]

-- Parser that always fails
failure :: Parser a
failure = \ inp -> []

-- Fails if string is empty, return' first char otherwise
item :: Parser Char
item = \ inp -> case inp of
  [] -> []
  (x:xs) -> [(x, xs)]

-- Abstract from the representation of parsers
parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

--
-- Sequencing
--

-- then operator
-- if parse p fails, everything fails
-- otherwise the result of parse p parsed with f
-- and that gives the final result

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \ inp -> case parse p inp of
  [] -> []
  [(v, out)] -> parse (f v) out

-- parse fir and third chars, omit second one
--p :: Parser (Char, Char)
p = do x <- item
       item
       y <- item
       return' (x, y)


--
-- Choice
--

-- or else operator
-- apply first parser to the input, and if it fails, apply second instead.

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \ inp -> case parse p inp of
  [] -> parse q inp
  [(v, out)] -> [(v, out)]

