module Address.Types
   (
     Component(..)
   , HouseNum(..)
   , Part(..)
   , isRoad
   , getRoad
   , isSettle
   , getSettle
) where


data Component =

    -- Settlement
      Город String
    | Посёлок String
    | Село String
    | Деревня String

    -- District
    | Микрорайон String

    -- Road
    | Улица String
    | Шоссе String
    | Переулок String
    | Бульвар String
    | Проспект String
    | Набережная String
    | Спуск String
    | Тупик String

    -- Digital
    | Дом HouseNum
    | Корпус HouseNum
    | Строение HouseNum
    | Владение HouseNum

    deriving (Show, Eq, Ord)


-- Числовое значение компоненты
data HouseNum = HouseNum Part (Maybe Part) deriving (Show, Eq, Ord)
data Part = Part Int (Maybe Char) deriving (Show, Eq, Ord)


-- Функции для поиска и извлечения названия дороги и поселения

isRoad (Улица _)       = True
isRoad (Шоссе _)       = True
isRoad (Переулок _)    = True
isRoad (Бульвар _)     = True
isRoad (Проспект _)    = True
isRoad (Набережная _)  = True
isRoad (Спуск _)       = True
isRoad _               = False

getRoad (Улица x)      = x
getRoad (Шоссе x)      = x
getRoad (Переулок x)   = x
getRoad (Бульвар x)    = x
getRoad (Проспект x)   = x
getRoad (Набережная x) = x
getRoad (Спуск x)      = x

isSettle (Город _)     = True
isSettle (Посёлок _)   = True
isSettle (Село _)      = True
isSettle (Деревня _)   = True

getSettle (Город x)    = x
getSettle (Посёлок x)  = x
getSettle (Село x)     = x
getSettle (Деревня x)  = x
