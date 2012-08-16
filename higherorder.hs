applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f  xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

{- another sort impl -}
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerpart = quicksort (filter (<x) xs)
        largerpart = quicksort (filter (>x) xs)
    in  smallerpart ++ [x] ++ largerpart

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 38299 == 0

{- generate a collatz sequence -}
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
    |even x = x:chain (x `div` 2)
    |odd x = x:chain (x*3 + 1)

{- find number of chains longer then 15 between with roots between
 - 1~100 -}
numofChains :: Int
numofChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

{- instead using a \ (lambda) -}
numofLong :: Int
numofLong = length (filter (\xs -> length xs > 15) (map chain [1..100]))

