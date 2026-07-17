multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

dividedByTen :: (Floating a) => a -> a
dividedByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])