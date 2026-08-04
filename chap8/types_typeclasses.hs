module Shapes (
    Point(..),
    Shape(..),
    surface,
    nudge,
    baseCircle,
    baseRect
) where

import Prelude hiding (lookup)
import qualified Data.Map as Map

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

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]
phoneBook :: PhoneBook
phoneBook = 
    [("amelia","555-2938")
    ,("freya","452-2928")
    ,("isabella","493-2928")
    ,("neil","205-2928")
    ,("roald","939-8282")
    ,("tenzing","853-2492")
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k, v)]

type IntMap v = Map.Map Int v
type IntMap2 = Map.Map Int

data Either2 a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Prelude.Nothing -> Prelude.Left $ "locker number " ++ show lockerNumber ++ " doesn't exist"
        Prelude.Just (state, code) -> if state /= Taken
                                then Prelude.Right code
                                else Prelude.Left $ "locker " ++ show lockerNumber ++ " is already taken"

lockers :: LockerMap  
lockers = Map.fromList  
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  