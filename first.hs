module Test where

double :: Num a => a -> a -> a
double x y = x*2 + y*2

doubleSmallNumber :: (Num a, Ord a) => a -> a
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

lucky :: (Integral a) => a -> String
lucky x = "Sorry, you're out of luck, pal!"

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

head1' :: [a] -> a
head1' xs = case xs of [] -> error "No head for empty list"
                       (x:_) -> x

length' :: (Integral b) => [a] -> b
length' list = sum [1 | _ <- list]

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi x y | (x, y) <- xs]
    where bmi weight height = (weight / height) ^ 2
