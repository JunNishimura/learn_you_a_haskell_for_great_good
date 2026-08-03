module Shapes (
    Point(..),
    Shape(..),
    surface,
    nudge,
    baseCircle,
    baseRect
) where

data Point = Point Float Float deriving (Show)
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

-- data Person = Person String String Int Float String String deriving (Show)
-- data Person = Person {
--     firstName :: String,
--     lastName :: String,
--     age :: Int,
--     height :: Float,
--     phoneNumber :: String,
--     flavor :: String
-- } deriving (Show)

-- data Car = Car String String Int deriving (Show)
-- data Car = Car {
--     company :: String,
--     model :: String,
--     year :: Int
-- } deriving (Show)

data Car a b c = Car {
    company :: a,
    model :: b,
    year :: c
} deriving (Show)

-- tellCar :: Car -> String
-- tellCar (Car {company = c, model = m, year = y}) = "This" ++ c ++ " " ++ m ++ " was made in " ++ show y

tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) = "This" ++ c ++ " " ++ m ++ " was made in " ++ show y

data Maybe a = Nothing | Just a

-- data (Ord k) = Map k v

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector(i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int
} deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)