module Main.Analysis where



import Data.Set ((\\))
import qualified Data.Set as Set
import Text.EditDistance
import Data.List (sortBy, find)
import Data.Tuple (swap)
import Text.Parsec.Error (ParseError)
import Data.Maybe (fromJust, isNothing)
import Data.Either (rights)
import Data.Char (toLower)
import Data.List (groupBy)
import Debug.Trace (trace)

import Address.Types (Component, isRoad, getRoad)



-- Нечёткий поиск подстроки.
--
-- Разбивает образец и проверяемую строку на слова. Далее слова образца 
-- сцепляет вместе, чтобы нормализовать пробелы. А из слов проверяемой строки 
-- сооружает множество словосечетаний длиной, но чтобы количество слов в них не 
-- превышало таковое в образце. Например для:
--     pattern = "a b c"
--     testing = "a b c d e"
-- Будут комбинации:
--     "a b c"
--     "b c d"
--     "c d e"
-- После этого применяет алгоритм Дамерау-Левенштейна образцу и каждому из 
-- словосочетаний. Возвращает минимальное из получившихся расстояний.

linearSearch :: String -> String -> Int
linearSearch pattern testing = minimum $ map (distance pattern') testing'
    where

        distance = restrictedDamerauLevenshteinDistance defaultEditCosts

        -- Нормализую пробелы
        pattern' = unwords (words pattern)

        -- Собираю возможные комбинации слов из проверяемой строки с тем же 
        -- количеством слов, что и в образце.
        testing' = flip map [0..v] $ \i -> unwords $ slice i p (words testing)
            where p = length (words pattern)
                  t = length (words testing)
                  v = notNegative $ t - (p - 1) -- количество комбинаций слов
                      where notNegative n | n < 0 = 0
                            notNegative n | otherwise = n
                  slice a b = take b . drop a



-- Количество адресов с парой
matchedCount ::
    [ (String, Either ParseError [Component]) ] ->
    [ (String, Either ParseError [Component]) ] ->
    Int
matchedCount xs ys = Set.size $ Set.intersection (toSet xs) (toSet ys)
    where toSet = Set.fromList . rights . snd . unzip



-- Формирует список дубликатов (одинаковое множество компонент).
-- Возвращает количество повторений каждого из дубликатов.
duplicates ::
    [ (String, Either ParseError [Component]) ] ->
    [ (Int, (String, Either ParseError [Component])) ]
duplicates = map (\xs -> (length xs, head xs))
           . filter ((>1) . length)
           . groupBy test
    where test (_, Right a) (_, Right b) = Set.fromList a == Set.fromList b
          test _ _ = False



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
                then Left "В адресе нет имени дороги!"
                else Right
                   $ sortBy (\ (_,x,_) (_,y,_) -> compare x y)
                   $ filter (\ (_,x,_) -> x < 3 )
                   $ for ys (\ (yStr, y) ->
                       ( yStr
                       , linearSearchIC (getRoad $ fromJust road) yStr
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
          linearSearchIC a b = linearSearch (map toLower a) (map toLower b)
              -- Регистронезависимый поиск подстроки
