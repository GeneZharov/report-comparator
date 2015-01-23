module Address.Types
   (
     Component(..)
   , HouseNum(..)
   , Part(..)
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

    -- Digital
    | Дом HouseNum
    | Корпус HouseNum
    | Строение HouseNum
    | Владение HouseNum

    deriving (Show)


data HouseNum = HouseNum Part (Maybe Part) deriving (Show)
data Part = Part Int (Maybe Char) deriving (Show)
