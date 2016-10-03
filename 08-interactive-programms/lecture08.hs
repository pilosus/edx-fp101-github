module Lecture8 where

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

-- go to the line specified by x and y coordinates
goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- write a string at the given position
writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

-- perform list of action in sequence
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

-- rewrite putStr using seqn
-- putStr xs = seqn [putChar x | x <- xs]

-- Game Of Life
width :: Int
width = 5

height :: Int
height = 5

type Board = [Pos]

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showcells :: Board -> IO ()
showcells b = seqn [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

-- neighbours of the given position
neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x - 1, y - 1), (x, y - 1),
                          (x + 1, y - 1), (x - 1, y),
                          (x + 1, y), (x - 1, y + 1),
                          (x, y + 1), (x + 1, y + 1)]

-- assume the board is a torus, i.e. each position has 8 neighbours
wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1,
                ((y - 1) `mod` height) + 1)
                

-- count the number of alive neighbours
liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

-- list of living positions with two or three liveing neighbours
survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

-- empty posns with 3 living neighbours
births :: Board -> [Pos]

{-|
-- inefficient for larger boards
births b = [(x,y) | x <- [1..width],
            y <- [1..height],
            isEmpty b (x,y),
            liveneighbs b (x,y) == 3]

-}
-- search onle between neighbours of the living cells
births b = [p | p <- rmdups (concat (map neighbs b)),
            isEmpty b p,
            liveneighbs b p == 3]

-- remove duplicates
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)

-- produce next generation of a board
nextgen :: Board -> Board
nextgen b = survivors b ++ births b

-- main function
life :: Board -> IO ()
life b = do cls
            showcells b
            wait 500000
            life (nextgen b)

-- counter
wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]
