module Data.Taskell.String where

backspace :: String -> String
backspace s = if not (null s) then init s else s
