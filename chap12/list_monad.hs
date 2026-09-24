-- instance Monad [] where
--     xs >>= f = concat (map f xs)

listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a', 'b']
    return (n, ch)

class Applicative f => Alternative' f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

instance Alternative' [] where
    empty = []
    (<|>) = (++)

guard' :: (Alternative' f) => Bool -> f ()
guard' True = pure ()
guard' False = empty

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard' ('7' `elem` show x)
    return x
