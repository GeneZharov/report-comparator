module Utils.DrawDir where


import System.File.Tree (getDirectory, toTree)
import Control.Monad

import Data.Tree


-- Представляет содержимое каталога в виде дерева
drawDir :: FilePath -> IO [String]
drawDir dir = (formatTree . toTree) `liftM` getDirectory dir


-- Формирует минималистичное строчное представление дерева
formatTree :: Tree String -> [String]
formatTree = draw . model 0
  where

    model :: Int -> Tree a -> [(Int, a)]
    model lvl (Node label forest) =
       (lvl, label) : concat (model (lvl + 1) `map` forest)

    draw :: [(Int, String)] -> [String]
    draw ls = [ indent ++ label
              | let shift = replicate 8 ' '
              , (lvl, label) <- ls
              , let indent = concat (replicate lvl shift)
              ]
