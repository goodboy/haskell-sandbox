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

{- gimmik to illustrate currying -}
addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z

{- demonstrate folding -}
{- a fold function takes in a binary operator, accumulator, and list.
 - it takes the first element of the list and operates on it via the
 - binary function and accum 
 - -> the following example folds from left to right -}
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

{- NOTE: this ^ can be simplified to
 - sum' :: (Num a) => [a] -> a
 - sum' = foldl (\acc x => acc + x) 0
 - -}

{- another example of map using foldr -}
map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr (\x acc -> f x : acc) [] xs

{- the more inefficient method using foldl
 - map2 :: (a -> b) -> [a] -> [b]
 - map2 f xs = foldl (\acc x -> acc ++ [f x]) [] xs
 - -}

{- example of foldl1 -> takes the head as acc by default -}
sum2' :: (Num a) => [a] -> a
sum2' = foldl1 (+)

{- std lib functions using folds -}
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

foldfilter' :: (a -> Bool) -> [a] -> [a]
foldfilter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

