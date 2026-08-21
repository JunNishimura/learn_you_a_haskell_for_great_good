import System.Environment
import System.IO
import System.IO.Error
import Control.Exception (catch)

main = toTry `catch` handler

toTry :: IO ()
toTry = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn $ "the file has " ++ show (length (lines contents)) ++ " lines"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of
            Just path -> putStrLn $ "whooops, file does not exist at: " ++ path
            Nothing -> putStrLn "whooops, file does not exist at unknown location"
    | otherwise = ioError e