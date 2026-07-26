import Data.List
-- import Data.List hiding (nub)
import qualified Data.Map as M
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifts = map (+ shift) ords
    in map chr shifts

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

phoneBook = [("amelia","555-2938"),("freya","452-2928"),("isabella","493-2928"),("neil","205-2928"),("roald","939-8282"),("tenzing","853-2492")]

-- findKey :: (Eq k) => k -> [(k,v)] -> v
-- findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
-- findKey key [] = Nothing
-- findKey key ((k,v):xs) = if key == k
--                             then Just v
--                             else findKey key xs
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

fromList' :: (Ord k) => [(k,v)] -> M.Map k v
fromList' = foldr (\(k,v) acc -> M.insert k v acc) M.empty

phoneBook2 = 
    [("amelia","555-2938")  
    ,("amelia","342-2492")  
    ,("freya","452-2928")  
    ,("isabella","493-2928")  
    ,("isabella","943-2929")  
    ,("isabella","827-9162")  
    ,("neil","205-2928")  
    ,("roald","939-8282")  
    ,("tenzing","853-2492")  
    ,("tenzing","555-2111")  
    ]
phoneBookToMap :: (Ord k) => [(k, a)] -> M.Map k [a]
-- phoneBookToMap xs = M.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs
phoneBookToMap xs = M.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs