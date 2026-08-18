import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "the arguments are:"
    mapM putStrLn args
    putStrLn "the program name is:"
    putStrLn progName
