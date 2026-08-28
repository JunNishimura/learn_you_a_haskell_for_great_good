-- main = do
--     line <- getLine
--     let line' = reverse line
--     putStrLn $ "you said " ++ line' ++ " backwards"
--     putStrLn $ "yes, you really said" ++ line' ++ " backwards"

main = do
    line <- fmap reverse getLine
    putStrLn $ "you said " ++ line ++ " backwards"
    putStrLn $ "yes, you really said" ++ line ++ "backwards"
