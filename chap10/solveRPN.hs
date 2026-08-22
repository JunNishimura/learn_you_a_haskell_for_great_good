import Data.List

solveRPN :: String -> Float
-- solveRPN expression = head (foldl foldingFunc [] (words expression))
--     where foldingFunc stack item =
solveRPN = head . foldl foldingFunc [] . words
    where foldingFunc (x:y:ys) "*" = (x * y):ys
          foldingFunc (x:y:ys) "+" = (x + y):ys
          foldingFunc (x:y:ys) "-" = (y - x):ys
          foldingFunc (x:y:ys) "/" = (y / x):ys
          foldingFunc (x:y:ys) "^" = (y ** x):ys
          foldingFunc (x:xs) "ln" = log x:xs
          foldingFunc xs "sum" = [sum xs]
          foldingFunc xs numberString = read numberString:xs