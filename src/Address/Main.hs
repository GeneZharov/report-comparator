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
parseAddr = either Left (Right . mergeLitera) . parse address ""


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


address = many space *> component `sepEndBy` sep <* eof
    where sep = optional (char ',' <|> char '.') *> many space


component = S.constant
        <|> try C.prefix
        <|> try C.postfix
        <|> try D.prefix
        <|> try S.prefix
        <|> try D.postfix
        <|>     S.postfix
        -- <|> anyChar *> component -- восстановление после ошибки в адресе
