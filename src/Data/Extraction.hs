-- Извлечение адресов из файлов и их разбор
module Data.Extraction where


import Debug.Trace (trace)
import Control.Monad
import System.FilePath (takeFileName, takeBaseName)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Text.Parsec.Error (ParseError)
import Control.Exception (throwIO)
import System.IO.Error (userError)
import Text.Regex.TDFA
import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.Process (proc)

import Paths_report_comparator
import Data.Types
import Address.Main
import Address.Types
import Utils.PythonStdout
import Utils.DrawDir


-- Извлекает адреса из имён файлов внутри каталога с фотографиями. Может 
-- работать в двух режимах: извлечение из имён каталогов и из имён обычных 
-- файлов.
fromPhotos :: Bool -> FilePath -> IO [Address]
fromPhotos dirMode dir = do

   dirContents <- getDirectoryContents dir
   let files = [ file
               | name <- dirContents
               , name /= "."
               , name /= ".."
               , let file = dir ++ "/" ++ name
               ]

   hasSubDirs <- (not . null) `liftM` (doesDirectoryExist `filterM` files)

   if hasSubDirs
   then concat `liftM` (fromPhotos dirMode `mapM` files)
   else if dirMode
        then
           -- Если в каталоге нет ни одного подкаталога,
           -- значит имя каталога содержит искомый адрес.
           let name = takeFileName dir
           in return [ Address name (Photos dir) dir ]
        else
           -- Если в каталоге нет ни одного подкаталога, значит адреса 
           -- содержатся в непосредственном содержимом каталога.
           return [ Address name (Photos file) file
                  | file <- files
                  , let name = cropCopy (takeBaseName file)
                  ]

   where cropCopy name = name'
            where (name', _, _) = name =~ " \\([[:digit:]]+\\)\
                                         \| - Copy \\([[:digit:]]+\\)\
                                         \| - Копия \\([[:digit:]]+\\)"
                                         :: (String, String, String)



-- Извлекает адреса из таблицы отчёта в заданном файле
fromNotes :: String -> Int -> FilePath -> IO [Address]
fromNotes sheet col file = do
   pyFile <- getDataFileName "tables/addresses"
   pyOut <- pythonStdout $ proc "python" [pyFile, file, sheet]
   return [ Address name (Notes file sheet col row) ctx
          | (row, line) <- [0..] `zip` splitOn "\0\n" pyOut
          , let names = lines line :: [String]
          , col < length names
          , let name  = names !! col
                ctx   = intercalate " | " names
          ]



genPhotosPreview :: FilePath -> IO String
genPhotosPreview dir = do
   model <- modelDir dir
   let (lastLevel, _) = model !! ((length model - 1) `min` (maxLines - 1))
   return $ drawDir ( take maxLines model ++ [(lastLevel, "...")] )
   where maxLines = 20



genNotesPreview :: String -> FilePath -> IO String
genNotesPreview sheet file = do
   pyFile <- getDataFileName "tables/addresses"
   pyOut  <- pythonStdout $ proc "python" [pyFile, file, sheet]
   let preview = [ ctx
                 | line <- splitOn "\0\n" pyOut
                 , let names = lines line :: [String]
                       ctx   = intercalate " | " names
                 ]
   return $ unlines (take maxLines preview ++ ["..."])
   where maxLines = 1000
