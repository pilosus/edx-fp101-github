-- https://www.seas.upenn.edu/~cis552/lectures/Concurrency.html
module Lab5 where

import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action 
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================
-- https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
-- ((a -> Action) -> Action) -> Action
-- function takes in:
--   a continuation function (a -> Action),
--  and returns Action
action :: Concurrent a -> Action
action (Concurrent m) = m $ \ _ -> Stop

-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = Concurrent $ \ _ -> Stop

-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
atom io = Concurrent $ \ c -> Atom (io >>= \ a -> return (c a))
--atom m = Concurrent $ \k -> -- k :: a -> Action
--  Atom $ liftM k m

-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork m = Concurrent $ \ k -> Fork (action m) (k ())

par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent a) (Concurrent b) = Concurrent $ \ k -> Fork (a k) (b k)

-- ===================================
-- Ex. 4
-- ===================================
-- https://stackoverflow.com/questions/27190934/implementation-for-poor-mans-concurrency-monad
runConcurrent                 :: Concurrent b -> (b -> Action) -> Action
runConcurrent (Concurrent h)  =  h

instance Monad Concurrent where
--    (Concurrent f) >>= g = Concurrent (\c -> f (\v -> runConcurrent (g v) c))
    (Concurrent f) >>= g = Concurrent (\c -> f (\v -> case g v of (Concurrent b) -> b c))
    return x = Concurrent (\c -> c x)


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin [] = return ()
roundRobin (Atom io : cs) = io >>= \ a -> roundRobin (cs ++ [a])
roundRobin (Fork a b : cs) = roundRobin (cs ++ [a,b])
roundRobin (Stop : cs) = roundRobin cs

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

