-- Извлечение адресов из файлов и их разбор
module Data.Extraction where


import Control.Monad
import System.FilePath (takeFileName, takeBaseName)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Text.Parsec.Error (ParseError)
import System.IO
import Debug.Trace (trace)
import Control.Exception (throwIO)
import System.IO.Error (userError)
import System.Environment (getEnvironment)
import System.Process

import Address.Main
import Address.Types


-- Утилита для получения стандартного вывода из дочернего процесса. Создавалась 
-- специально для питона, чтобы принудить его писать в utf-8, так как в windows 
-- питон сбоит из-за того, что не умеет писать в дефолтной системной кодировке.
pythonStdout :: CreateProcess -> IO String
pythonStdout conf = do
    env <- getEnvironment
    (_, Just outH, _, _) <- createProcess conf
        {
            std_out = CreatePipe
        ,   env = Just (("PYTHONIOENCODING", "utf_8"):env)
        }
    hSetEncoding outH utf8
    hGetContents outH


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
{- Извлечение из CSV
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
-}
fromNotes :: String -> Int -> String -> IO [String]
fromNotes sheet col file = liftM lines $ pythonStdout
    (proc "python" ["./Data/tables/addresses", file, sheet, show col])



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
