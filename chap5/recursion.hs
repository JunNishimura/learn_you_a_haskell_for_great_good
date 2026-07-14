maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

minimum' :: (Ord a) => [a] -> a
minimum' [] = error "minimum of empty list"
minimum' [x] = x
minimum' (x:xs)
    | x < minTail = x
    | otherwise = minTail
    where minTail = minimum' xs