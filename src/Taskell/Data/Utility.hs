module Taskell.Data.Utility where

updateLast :: (a -> a) -> [a] -> [a]
updateLast _ [] = []
updateLast f [k] = [f k]
updateLast f (y : ys) = y : updateLast f ys