module Address.Types
   (
     Component(..)
   , HouseNum(..)
   , Part(..)
   , isRoad
   , getRoad
) where


data Component =

    Область String

    -- Settlement
    | Город String
    | Посёлок String
    | Село String
    | Деревня String

    -- District
    | Район String
    | Микрорайон String

    -- Road
    | Улица String
    | Шоссе String
    | Переулок String
    | Бульвар String
    | Проспект String
    | Набережная String
    | Проезд String
    | Спуск String
    | Тупик String

    -- Char
    | Литера Char

    -- Digital
    | Дом HouseNum
    | Корпус HouseNum
    | Строение HouseNum
    | Владение HouseNum

    deriving (Show, Eq, Ord)


-- Числовое значение компоненты
data HouseNum = HouseNum Part (Maybe Part) deriving (Show, Eq, Ord)
data Part = Part Int (Maybe Char) deriving (Show, Eq, Ord)


-- Функции для поиска и извлечения названия дороги

isRoad (Улица _)      = True
isRoad (Шоссе _)      = True
isRoad (Переулок _)   = True
isRoad (Бульвар _)    = True
isRoad (Проспект _)   = True
isRoad (Набережная _) = True
isRoad (Проезд _)     = True
isRoad (Спуск _)      = True
isRoad (Тупик _)      = True
isRoad _              = False

getRoad (Улица x)      = x
getRoad (Шоссе x)      = x
getRoad (Переулок x)   = x
getRoad (Бульвар x)    = x
getRoad (Проспект x)   = x
getRoad (Набережная x) = x
getRoad (Проезд x)     = x
getRoad (Спуск x)      = x
getRoad (Тупик x)      = x
