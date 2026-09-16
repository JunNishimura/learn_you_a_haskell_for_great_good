class Applicative m => Monad' m where
    return :: a -> m a
    return = pure

    (>>==) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    x >> y = x >>== \_ -> y

instance Monad' Maybe where
    Nothing >>== f = Nothing
    Just x >>== f = f x