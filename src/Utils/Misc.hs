module Utils.Misc where


import Data.List


-- Комбинирует элементы списка, если они удовлетворяют критерию
-- Например: combine (==) [1, 2, 1, 3, 4, 2, 1] -- [[1,1,1],[2,2],[3],[4]]
combine :: (a -> a -> Bool) -> [a] -> [[a]]
combine f (x:xs) =
   let (dups, rest) = partition (f x) xs
   in (x:dups) : combine f rest
combine _ [] = []
