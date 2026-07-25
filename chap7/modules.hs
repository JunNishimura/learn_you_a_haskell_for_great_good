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