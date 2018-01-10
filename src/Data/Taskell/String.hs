module Data.Taskell.String (
    backspace,
    trunc,
    wrap
) where

import Data.List (foldl')
import Data.List.Split (split, oneOf, keepDelimsL)

backspace :: String -> String
backspace s = if not (null s) then init s else s

trunc :: Int -> String -> String
trunc width s = if length s > width then take (width - 3) s ++ "..." else s

trim :: String -> String
trim = unwords . words

-- wrapping
spl :: String -> [String]
spl = split (keepDelimsL $ oneOf " ")

wrap :: Int -> String -> [String]
wrap width = foldl' (combine width) [] . spl

combine :: Int -> [String] -> String -> [String]
combine width acc s = if nl then acc ++ [trim s] else append (l ++ s) acc
    where l = if null acc then "" else last acc
          nl = length l + length s > width

append :: String -> [String] -> [String]
append s l = l' ++ [s]
    where l' = if null l then l else init l
