module Data.Types where
import Text.Parsec.Error (ParseError)
import Address.Types (Component)


type ErrMsg   = String


data Type = Photo | Note deriving (Show)


data Origin = PhotoOrigin FilePath
            | NoteOrigin  FilePath
                          String -- имя страницы
                          Int    -- столбец
                          Int    -- строка
              deriving (Show)


data Address = Address {
     addressString  :: String
        -- Оригинал адреса из имени файла или строки в отчёте
   , addressOrigin  :: Origin
        -- Происхождение адреса
   , addressContext :: String
        -- Строка отчёта или полный путь до файла фотографии
   } deriving (Show)


data Parsed = Parsed {
     parsedAddress :: Address
   , parsedComps   :: Either ParseError [Component]
   } deriving (Show)
