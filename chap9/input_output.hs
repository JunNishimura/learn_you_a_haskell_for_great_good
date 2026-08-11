import Control.Monad
import Text.XHtml (color)
-- import Data.Char

-- main = do
--     putStrLn "hello, what's your name?"
--     name <- getLine
--     putStrLn ("hey " ++ name ++ ", you rock!")

-- main = do
--     putStrLn "what's your first name?"
--     firstName <- getLine
--     putStrLn "what's your last name?"
--     lastName <- getLine
--     let bigFirstName = map toUpper firstName
--         bigLastName = map toUpper lastName
--     putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- main = do
--     a <- return "hello"
--     b <- return ", world"
--     putStrLn $ a ++ b

-- main = do
--     let a = "hello"
--         b = ", world"
--     putStrLn $ a ++ b

-- main = do
--     putStr "hey "
--     putStr "i'm "
--     putStrLn "andy"

-- main = do
--     putChar 't'
--     putChar 'e'
--     putChar 'h'

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs

-- main = do
--     print True
--     print 2
--     print "haha"
--     print 3.2
--     print [1,2,3]

-- main = do
--     c <- getChar
--     if c /= ' '
--         then do
--             putChar c
--             main
--         else return ()

-- main = do
--     c <- getChar
--     when (c /= ' ') $ do
--         putChar c
--         main

-- main = do
--     rs <- sequence [getLine, getLine, getLine]
--     print rs

-- main = forever $ do
--     putStr "give me some input: "
--     l <- getLine
--     putStrLn $ map toUpper l

main = do
    colors <- forM [1..4] (\a -> do
        putStrLn $ "which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "the colors that you associate with 1,2,3 and 4 are: "
    mapM putStrLn colors