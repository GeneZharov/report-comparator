import Main.Extraction (extract, fromNotes, fromPhotos)
import Main.Analysis (notParsed, notMatched)


main :: IO ()
main = do

    notes  <- extract fromNotes  "/root/s/zdrav/csv/from-excel.csv"
    photos <- extract fromPhotos "/root/s/zdrav/отчёты/Фото Эпилепсия 09.2014"
    --print notes
    --print photos

    -- Адреса, которые не удалось распарсить
    --print $ notParsed notes
    --print $ notParsed photos

    -- Адреса отчёта без пары
    print $ notMatched notes photos
    --print $ notMatched photos notes
