import Data.Char
import Control.Monad -- filterM

beep :: IO ()
beep = putStr "\BEL"

-- perform list of action in sequence
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

-- ex 1
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

-- ex 2
putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

putStrLn'' :: String -> IO ()
putStrLn'' [] = putChar '\n'
putStrLn'' xs = putStr' xs >> putChar '\n'

putStrLn''' :: String -> IO ()
putStrLn''' [] = putChar '\n'
putStrLn''' xs = putStr' xs >>= \ x -> putChar '\n'

putStrLn4 :: String -> IO ()
putStrLn4 [] = putChar '\n'
putStrLn4 xs = putStr' xs >> putStr' "\n"

-- ex 3
getLine' :: IO String
getLine' = get ""
get :: String -> IO String
get xs = do x <- getChar
            case x of
              '\n' -> return xs
              _ -> get (xs ++ [x])

-- ex 4
str2upper :: String -> String
str2upper xs = map (toUpper) xs

interact' :: (String -> String) -> IO ()
interact' f = do input <- getLine'
                 putStrLn' (f input)

--ex 5
-- seqn [putChar x | x <- "hello world"]
sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (m : ms) = (foldl (>>) m ms) >> return ()

sequence_'' :: Monad m => [m a] -> m ()
sequence_'' [] = return ()
sequence_'' (m : ms) = m >> sequence_' ms

sequence_''' :: Monad m => [m a] -> m ()
sequence_''' [] = return ()
sequence_''' (m : ms) = m >>= \ _ -> sequence_' ms

sequence_4 :: Monad m => [m a] -> m ()
sequence_4 ms = foldr (>>) (return ()) ms

-- ex 6
-- take list monadic values, and evaluate them in sequence, from left to right,
-- collecting all (intermediate) results into a list
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = m >>=
                   \ a ->
                     do as <- sequence' ms
                        return (a:as)

sequence'' :: Monad m => [m a] -> m [a]
sequence'' ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc
      = do x <- m
           xs <- acc
           return (x:xs)

sequence''' :: Monad m => [m a] -> m [a]
sequence''' [] = return []
sequence''' (m:ms)
  = do a <- m
       as <- sequence''' ms
       return (a:as)

-- ex 7
-- take a function of type a -> m b, and a finite list of  elements of type a
-- and (similarly to map) applies the function to every element of the list,
-- but produces the resulting list wrapped inside a monadic action

-- https://stackoverflow.com/questions/3137162/map-versus-mapm-behavior
-- mapM f as = sequence (map f as)
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f as = sequence' (map f as)

mapM'' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'' f [] = return []
mapM'' f (a:as)
  = f a >>= \ b -> mapM'' f as >>= \ bs -> return (b:bs)

                                            
mapM''' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM''' f [] = return []
mapM''' f (a:as) =
  do
    b <- f a
    bs <- mapM''' f as
    return (b:bs)


mapM4 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM4 f [] = return []
mapM4 f (a:as)
  = f a >>=
    \ b ->
    do bs <- mapM4 f as
       return (b:bs)

-- ex 8
-- import Control.Monad
-- https://byorgey.wordpress.com/2007/06/26/deducing-code-from-types-filterm/
-- filterM (const [True,False]) "123"
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x:xs)
  = do flag <- p x
       ys <- filterM' p xs
       if flag then return (x:ys) else return ys

-- ex 9
{-
foldl f a []     = a
foldl f a (x:xs) = foldl f (f a x) xs
-}

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
--foldLeftM f a (x:xs) = do a' <- f a x
--                          foldLeftM f a' xs
foldLeftM f a (x:xs) = f a x >>= \ fax -> foldLeftM f fax xs 

{-
foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM acc a [] = return a
foldLeftM acc a (b : bs) = do a' <- acc a b
                              foldLeftM acc a' bs
-}

-- foldLeftM (\ a b -> putChar b >> return (b:a ++ [b])) [] "haskell" >>= \r -> putStrLn r

-- ex 10
foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f a []      =  return a
foldRightM f a (x:xs)  =  foldRightM f a xs >>= \ fax -> f x fax

-- foldRightM (\ a b -> putChar a >> return (a:b)) [] (show [1,3..10]) >>= \r -> putStrLn r

-- ex 11
-- liftM (map toUpper) getLine
-- Hello
-- HELLO
liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f m = m >>= \a -> return (f a)

liftM'' :: Monad m => (a -> b) -> m a -> m b
liftM'' f m = do x <- m
                 return (f x)
