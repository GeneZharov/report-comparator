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
import Text.Regex.TDFA
import Control.Monad.IO.Class

import Paths_report_comparator
import Data.Types
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
fromPhotos :: Bool -> FilePath -> IO [Address]
fromPhotos dirMode dir = do

    dirContents <- getDirectoryContents dir
    let names = [ file
                   | name <- dirContents
                   , name /= "."
                   , name /= ".."
                   , let file = dir ++ "/" ++ name
                   ]

    hasSubDirs <- (not . null) `liftM` (doesDirectoryExist `filterM` names)

    if hasSubDirs
    then concat `liftM` (fromPhotos dirMode `mapM` names)
    else if dirMode
         then return [takeFileName dir]
             -- Если в каталоге нет ни одного подкаталога,
             -- значит имя каталога содержит искомый адрес.
         else return $ (cropCopy . takeBaseName) `map` names
             -- Если в каталоге нет ни одного подкаталога, значит адреса 
             -- содержатся в непосредственном содержимом каталога.

    where cropCopy name = name'
              where (name', _, _) = name =~ " \\([[:digit:]]+\\)\
                                           \| - Copy \\([[:digit:]]+\\)\
                                           \| - Копия \\([[:digit:]]+\\)"
                                           :: (String, String, String)



-- Извлекает адреса из таблицы отчёта в заданном файле
fromNotes :: String -> Int -> FilePath -> IO [Address]
fromNotes sheet col file = do
    py <- getDataFileName "tables/addresses"
    lines `liftM` pythonStdout (proc "python" [py, file, sheet, show col])



-- Извлекает адреса с помощью пользовательской функции из заданного файла и 
-- возвращает список пар со строкой адреса и результатом его разбора.
extract :: (FilePath -> IO [Address])
        -> FilePath
        -> IO [ (Address, Either ParseError [Component]) ]
extract extracter f = do
    strings <- extracter f
    return [ (string, parsed)
           | string <- strings
           , let parsed = parseAddr string
           ]
