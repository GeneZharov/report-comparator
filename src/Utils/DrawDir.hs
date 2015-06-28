module Utils.DrawDir where


import System.File.Tree (getDirectory, toTree)
import Control.Monad
import Data.Tree


modelDir :: FilePath -> IO [(Int, String)]
modelDir dir = (modelDir' 0 . toTree) `liftM` getDirectory dir
   where
      modelDir' :: Int -> Tree String -> [(Int, String)]
      modelDir' lvl (Node label forest) =
         (lvl, label) : modelDir' (lvl + 1) `concatMap` forest



drawDir :: [(Int, String)] -> String
drawDir lines = unlines [ indent ++ label
                        | let shift = replicate 8 ' '
                        , (lvl, label) <- lines
                        , let indent = concat (replicate lvl shift)
                        ]
