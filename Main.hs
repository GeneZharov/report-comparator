import ParseCSV
import Address.Main
import Text.Regex.PCRE ((=~))
import System.Directory (getDirectoryContents)
import System.FilePath (takeBaseName)



-- Извлекает адреса из таблицы отчёта в заданном файле
fromNotes :: String -> IO [String]
fromNotes file = readFile file
         >>= return . parseCSV
         >>= either (return . error . show) (return . pick)
    where pick lines = lines >>=
              \l -> if head l =~ "\\d+" -- Если в первой ячейке номер строки,
                    then [l!!2]         -- то адрес лежит в 3-й ячейке.
                    else []             -- Иначе строка не содержит адрес.



-- Извлекает адреса из имён файлов фотографий в заданном каталоге
fromPhotos :: String -> IO [String]
fromPhotos dir =
    getDirectoryContents dir
    >>= return . map takeBaseName . filter (`notElem` [ ".", ".." ])



main :: IO ()
main = do

    -- Извлекаю адреса из фотографий и отчёта
    notes  <- fromNotes "./_sources/from-excel.csv"
    photos <- fromPhotos "/root/s/zdrav/отчёты/Фото Эпилепсия 09.2014"

    print $ map parseAddr notes
