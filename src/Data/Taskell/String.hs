module Data.Taskell.String (
    backspace,
    trunc,
    wrap
) where

backspace :: String -> String
backspace s = if not (null s) then init s else s

trunc :: Int -> String -> String
trunc width s = if length s > width then take (width - 3) s ++ "..." else s

-- wrapping
wrap :: Int -> String -> [String]
wrap width = foldl (combine width) [] . words 

combine :: Int -> [String] -> String -> [String]
combine width acc s = if nl then acc ++ [s] else append (l ++ s) acc 
    where l = if null acc then "" else last acc ++ " "
          nl = length l + length s > width

append :: String -> [String] -> [String]
append s l = l' ++ [s]
    where l' = if null l then l else init l
