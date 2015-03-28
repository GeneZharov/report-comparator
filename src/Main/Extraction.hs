-- Извлечение адресов из файлов и их разбор
module Main.Extraction where


import Control.Monad
import Text.Regex.PCRE ((=~))
import System.FilePath (takeFileName, takeBaseName)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Text.Parsec.Error (ParseError)
import System.IO
import Debug.Trace (trace)
import Control.Exception (throwIO)
import System.IO.Error (userError)

import Address.Main
import Address.Types
import ParseCSV


-- Извлекает адреса из имён файлов внутри каталога с фотографиями. Может 
-- работать в двух режимах: извлечение из имён каталогов и из имён обычных 
-- файлов.
fromPhotos :: Bool -> String -> IO [String]
fromPhotos dirMode dir = do

    names <- liftM (filter (`notElem` [ ".", ".." ])) (getDirectoryContents dir)
    let fullNames = map ((dir ++ "/") ++) names
    hasSubDirs <- liftM (not . null) (filterM doesDirectoryExist fullNames)

    if hasSubDirs
    then liftM concat $ mapM (fromPhotos dirMode) fullNames
    else if dirMode
         then return [takeFileName dir]
             -- Если в каталоге нет ни одного подкаталога,
             -- значит имя каталога содержит искомый адрес.
         else return (map takeBaseName names)
             -- Если в каталоге нет ни одного подкаталога, значит адреса 
             -- содержатся в непосредственном содержимом каталога.



-- Извлекает адреса из таблицы отчёта в заданном файле
fromNotes :: String -> IO [String]
fromNotes file = liftM parseCSV (readFile' utf8 file)
             >>= either (throwIO . userError . show) (return . pick)
    where pick lines = lines >>=
              \l -> if head l =~ "\\d+" -- Если в первой ячейке номер строки,
                    then [l!!2]         -- то адрес лежит в 3-й ячейке.
                    else []             -- Иначе строка не содержит адрес.
          readFile' :: TextEncoding -> String -> IO String
          readFile' enc name = do
              h <- openFile name ReadMode
              hSetEncoding h enc
              hGetContents h



-- Извлекает адреса с помощью пользовательской функции из заданного файла и 
-- возвращает список пар, где в каждой паре строка адреса и результат её 
-- разбора.
extract :: (String -> IO [String])
        -> String
        -> IO [ (String, Either ParseError [Component]) ]
extract extracter f = do
    strings  <- extracter f -- Извлекаю адреса
    let parsed = map parseAddr strings -- Анализирую адреса
    return $ zip strings parsed
        -- Возвращаю комбинацию строки адреса и результата её разбора
