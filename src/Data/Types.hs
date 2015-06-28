module Data.Types where
import Text.Parsec.Error (ParseError)
import Address.Types (Component)


type ErrMsg   = String


data Address = Address {
     addressReal    :: String -- оригинал адреса из имени файла или строки в отчёте
   , addressFake    :: String -- отредактированный пользователем оригинал адреса
   , addressContext :: String -- строка отчёта или полный путь до файла фотографии
   } deriving (Show)


data Parsed = Parsed {
     parsedData  :: Address
   , parsedValue :: Either ParseError [Component]
   } deriving (Show)
