foo :: Maybe String
-- foo = Just 3 >>= (\x -> 
--       Just "!" >>= (\y ->
--       Just (show x ++ y)))
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)

class Monad m => MonadFail' m where
    fail :: String -> m a

instance MonadFail' Maybe where
    fail _ = Nothing

wopwop :: Maybe Char  
wopwop = do  
    (x:xs) <- Just ""  
    return x
