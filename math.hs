mean :: (Fractional b) =>  [b] -> b
mean [] = error "passed an empty list"
mean x = sum x / (fromIntegral (length x))

rms :: (Floating a) => [a] -> a
rms [] = error "list is empty"
rms x = sqrt (mean (map (**2) x))

db :: (Floating a) => [a] -> a
db (x:[]) = 20 * logBase 10 (x)
db x = 20 * logBase 10 (rms x)

innerProduct :: (Num a) => [a] -> [a] -> a
innerProduct _ [] = 0
innerProduct [] _ = 0
innerProduct (x:xs) (y:ys) = x * y + innerProduct xs ys
