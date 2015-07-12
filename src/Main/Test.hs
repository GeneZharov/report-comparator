import Control.Monad (mapM_)

import Data.Types
import Data.Extraction
import Utils.ToReadable


main :: IO ()
main = do
   --print =<< fromPhotos False "/_reports/aqua"
   mapM_ (putStrLn . toReadable . addressString)
      =<< fromNotes "Лист1" 2 "/f/_reports/Отчет Аква Медиа 07.2015.xlsx"
   --print =<< fromNotes "ФРИСОЛАК" 3 "/_reports/2014.09.15.xls"
