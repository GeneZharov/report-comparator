module Address.Main where


import Text.Parsec
import Control.Applicative hiding (optional, (<|>), many)
import Data.List (find)
import Debug.Trace (trace)

import Address.Types
import qualified Address.Digit as D
import qualified Address.Symbol as S
import qualified Address.Char as C


parseAddr :: String -> Either ParseError [Component]
parseAddr = either Left (Right . mergeLitera) . runParser address False ""
    -- False — это user data, который показывает была ли уже распарсена хотя бы 
    -- раз компонента дороги. Если так, то можно трактовать номера без ключа 
    -- как номер дома.


-- Пост-обработка результата разбора: удаляет компоненту "Литера", а букву из 
-- неё вносит в компоненту "Дом", если в нём ещё нет литеры.
mergeLitera :: [Component] -> [Component]
mergeLitera cs = maybe cs edit . find isLitera $ cs
    where

        edit :: Component -> [Component]
        edit (Литера l) = filter (not . isLitera) . map (editHouse l) $ cs

        -- Предикат для распознавания компоненты "Литера"
        isLitera :: Component -> Bool
        isLitera (Литера _) = True
        isLitera _ = False

        -- Добавляет в компоненту "Дом" литеру
        editHouse :: Char -> Component -> Component
        editHouse l (Дом (HouseNum (Part n Nothing) Nothing))
                  = Дом $ HouseNum (Part n $ Just l) Nothing
        editHouse l x = x


address :: Parsec String Bool [Component]
address = many space *> component `sepEndBy` sep <* eof
    where sep = optional (char ',' <|> char '.') *> many space


component :: Parsec String Bool Component
component = try S.constant

        <|> try D.prefix
        <|> try S.prefix
        <|> try C.prefix -- редко используется

        <|> try D.postfix
        <|> try S.postfix
        <|> try C.postfix -- редко используется

        <|> try D.standalone
        <|>     S.standalone

        -- <|> anyChar *> component -- восстановление после ошибки в адресе
