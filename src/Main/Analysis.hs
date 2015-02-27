module Main.Analysis where



import Data.Set ((\\))
import qualified Data.Set as Set
import Text.EditDistance
import Data.List (sortBy, find)
import Data.Tuple (swap)
import Text.Parsec.Error (ParseError)
import Data.Maybe (fromJust, isNothing)
import Data.Either (rights)

import Address.Types (Component, isRoad, getRoad)



-- Вычисляет расстояние Дамерау-Левенштейна до каждого слова и возвращает 
-- минимальное расстояние.
linearSearch :: String -> String -> Int
linearSearch pattern str = minimum $ map (distance pattern) (words str)
    where distance = restrictedDamerauLevenshteinDistance defaultEditCosts



-- Количество адресов с парой
matchedCount ::
    [ (String, Either ParseError [Component]) ] ->
    [ (String, Either ParseError [Component]) ] ->
    Int
matchedCount xs ys = Set.size $ Set.intersection (toSet xs) (toSet ys)
    where toSet = Set.fromList . rights . snd . unzip



-- Формирует список не распарсенных адресов в группе
notParsed :: [ (String, Either ParseError [Component]) ]
          -> [ (String, Either ParseError [Component]) ]
notParsed xs = filter (isLeft . snd) xs
    where isLeft (Left _) = True
          isLeft _ = False



-- Для распарсенных адресов первой группы, которым не нашлось пары во второй 
-- группе, нахожу степень похожести на каждый из адресов второй группы.

notMatched ::
    [ (String, Either ParseError [Component]) ] ->
    [ (String, Either ParseError [Component]) ] ->
    [(
        String,      -- Адрес первой группы
        [Component], -- Распарсенный адрес первой группы
        Either String [(
            String -- Один из адресов второй группы
         ,  Int    -- Степень соответствия адресу первой группы
         ,  Bool   -- Есть ли уже у этого адреса пара из первой группы
        )]
    )]

notMatched xs ys =

    let

        -- Множества разобранных адресов.
        -- С ними будет удобно работать комбинаторами из пакета Data.Set
        toSet = Set.fromList . rights . snd . unzip
        xSet  = toSet xs
        ySet  = toSet ys

        -- Ассоциативный список для поиска строки адреса первой группы по 
        -- его распарсенной версии.
        xs' = map ( \ (str, Right parsed) -> (str, parsed) )
            $ filter (isRight . snd) xs

        -- Адреса 1-й группы, которым есть пара во 2-й
        intersection = Set.intersection xSet ySet

    in for
        (Set.toList $ xSet \\ ySet) -- Адреса 1-й группы без пары во 2-й
        (\ x -> (,,)
            (fromJust $ rlookup x xs') -- Строка с адресом
            x                          -- Распарсенные компоненты
            (let road = find isRoad x  -- Список похожих альтернатив
             in if isNothing road
                then Left "В адресе не задана дорога"
                else Right
                   $ sortBy (\ (_,x,_) (_,y,_) -> compare x y)
                   $ filter (\ (_,x,_) -> x < 3 )
                   $ for ys (\ (yStr, y) ->
                       ( yStr
                       , linearSearch (getRoad $ fromJust road) yStr
                       , either
                             (const False)
                             (`elem` Set.toList intersection)
                             y
                       )
                   )
            )
        )

    where for = flip map
          rlookup x = lookup x . map swap
          isRight (Right _) = True
          isRight _         = False
