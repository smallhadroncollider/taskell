module UI.Task where

import Graphics.Vty
import Data.Taskell.Task (Task, description)
import Config (width)

import UI.Styles

trunc :: String -> String
trunc s = if length s > width then take (width - 3) s ++ "..." else s

format :: Image -> Image
format img = marginTop $ if imageWidth img < width then resizeWidth width img else img

append :: String -> [String] -> [String]
append s l = l' ++ [s]
    where l' = if null l then l else init l

combine :: [String] -> String -> [String]
combine acc s = if nl then acc ++ [s] else append (l ++ s) acc 
    where l = if null acc then "" else last acc ++ " "
          nl = length l + length s > width

split :: String -> [String]
split = foldl combine [] . words 

-- style a task
present :: Bool -> Int -> Int -> Task -> Image
present current index i t = format img 
    where
        s = description t
        style = if current && index == i then attrCurrent else attrTask 
        img = vertCat $ map (string style) $ split s 
