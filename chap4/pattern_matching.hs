import Distribution.Compiler (CompilerFlavor(YHC))
import Text.XHtml (caption)
lucky :: (Integral a) => a -> String
lucky 7 = "lucky number seven"
lucky x = "sorry, you're out of lock, pal"

sayMe :: (Integral a) => a -> String
sayMe 1 = "one"
sayMe 2 = "two"
sayMe 3 = "three"
sayMe 4 = "four"
sayMe 5 = "five"
sayMe x = "not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "albert"
charName 'b' = "broseph"
charName 'c' = "cecil"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "can't call head on an empty list"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = error "can't call tail on an empty list"
tail' (_:xs) = xs

tell :: (Show a) => [a] -> String
tell [] = "the list is empty"
tell (x:[]) = "the list has one element: " ++ show x
tell (x:y:[]) = "the list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "this list is long. the first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

last' :: [a] -> a
last' [] = error "can't call last on an empty list"
last' (x:[]) = x
last' (_:xs) = last' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "empty string"
capital all@(x:xs) = "the first letter of " ++ all ++ " is " ++ [x]

densityTell :: (RealFloat a) => a -> String
densityTell density
    | density < 1.2 = "wow, you're going for a ride in the sky"
    | density <= 1000.0 = "have fun swimming but watch out for sharks"
    | otherwise = "if it's sink or swim, you're going to sink"

densityTell' :: (RealFloat a) => a -> a -> String
densityTell' mass volume
    | mass / volume < 1.2 = "wow, you're going for a ride in the sky"
    | mass / volume <= 1000.0 = "have fun swimming but watch out for sharks"
    | otherwise = "if it's sink or swim, you're going to sink"

densityTell'' :: (RealFloat a) => a -> a -> String
densityTell'' mass volume
    | density < air = "wow, you're going for a ride in the sky"
    | density <= water = "have fun swimming but watch out for sharks"
    | otherwise = "if it's sink or swim, you're going to sink"
    where density = mass / volume
          (air, water) = (1.2, 1000.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density m v | (m, v) <- xs]
    where density mass volume = mass / volume

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

head'' :: [a] -> a
head'' xs = case xs of [] -> error "no head for empty lists"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "the list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list"
                                               xs -> "a longer list"

describeList' :: [a] -> String
describeList' xs = "the list is " ++ what xs
    where what [] = "emtpy"
          what [x] = "a singleton list"
          what xs = "a longer list"