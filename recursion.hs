module Test where

-- Recursion

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- Using max
maximum1 :: (Ord a) => [a] -> a
maximum1 [] = error "maximum of empty list"
maximum1 [x] = x
maximum1 (x:xs) = max x (maximum1 xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' i (x:xs)
    | i < 1 = []
    | otherwise = x:take' (i-1) xs