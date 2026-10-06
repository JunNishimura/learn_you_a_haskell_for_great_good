import Data.Monoid
import Control.Monad.Writer

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

-- newtype Writer' w a = Writer' { runWriter :: (a, w) }
-- instance (Monoid w) => Monad (Writer' w) where
--     return x = Writer' (x, mempty)
--     (Writer' (x, v)) >>= f = let (Writer' (y, v')) = f x in Writer' (y, v <> v')

-- logNumber :: Int -> Writer [String] Int
-- logNumber x = Writer (x, ["Got number: " ++ show x])

-- multWithLog :: Writer [String] Int
-- multWithLog = do
--     a <- logNumber 3
--     b <- logNumber 5
--     tell ["gonna multiply these two"]
--     return (a * b)

-- gcd' :: Int -> Int -> Int
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    -- | b == 0 = a
    -- | otherwise = gcd' b (a `mod` b)
    | b == 0 = do
        tell ["finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)
    
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result
    
newtype DiffList a = DiffList { getDiffList :: [a] -> [a]}
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

-- instance Monoid (DiffList a) where
--     mempty = DiffList (\xs -> [] ++ xs)
--     (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

-- gcd'' :: Int -> Int -> Writer (DiffList String) Int
-- gcd'' a b
--     | b == 0 = do
--         tell (toDiffList ["finished with " ++ show a])
--         return a
--     | otherwise = do
--         result <- gcd'' b (a `mod` b)
--         tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
--         return result
    
-- finalCountDown :: Int -> Writer (DiffList String) ()
-- finalCountDown 0 = do
--     tell (toDiffList ["0"])
-- finalCountDown x = do
--     finalCountDown (x-1)
--     tell (toDiffList [show x])