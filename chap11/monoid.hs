import qualified Data.Foldable as F

class Monoid' m where
    mempty' :: m
    (<<>>) :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr (<<>>) mempty'

instance Monoid' [a] where
    mempty' = []
    (<<>>) = (++)

newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Product a) where
    mempty' = Product 1
    Product x <<>> Product y = Product (x * y)

newtype Sum a = Sum { getSum :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Sum a) where
    mempty' = Sum 0
    Sum x <<>> Sum y = Sum (x + y)

newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid' Any where
    mempty' = Any False
    Any x <<>> Any y = Any (x || y)

newtype All = All { getAll :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid' All where
    mempty' = All True
    All x <<>> All y = All (x && y)

instance Monoid' Ordering where
    mempty' = EQ
    LT <<>> _ = LT
    EQ <<>> y = y
    GT <<>> _ = GT

lengthCompare :: String -> String -> Ordering
-- lengthCompare x y = let a = length x `compare` length y
--                         b = x `compare` y
--                     in if a == EQ then b else a
-- lengthCompare x y = (length x `compare` length y) <>
--                     (x `compare` y)
lengthCompare x y = (length x `compare` length y) <>
                    (vowels x `compare` vowels y) <>
                    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")

instance Monoid' a => Monoid' (Maybe a) where
    mempty' = Nothing
    Nothing <<>> m = m
    m <<>> Nothing = m
    Just m1 <<>> Just m2 = Just (m1 <<>> m2)

newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid' (First a) where
    mempty' = First Nothing
    First (Just x) <<>> _ = First (Just x)
    First Nothing <<>> x = x

newtype Last a = Last { getLast :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid' (Last a) where
    mempty' = Last Nothing
    _ <<>> Last (Just x) = Last (Just x)
    x <<>> Last Nothing = x

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l <>
                             f x           <>
                             F.foldMap f r