module Data.Analysis where



import Text.EditDistance
import Data.List (find, groupBy, deleteFirstsBy, sortBy)
import Data.Maybe (fromJust)
import Data.Either (rights)
import Data.Char (toLower)
import qualified Data.Set as Set

import Data.Types
import Address.Types (Component, isRoad, getRoad)



toSet :: [Parsed] -> Set.Set [Component]
toSet = Set.fromList . rights . map parsedComps



sameAddr :: Parsed -> Parsed -> Bool
sameAddr (Parsed _ (Right as))
         (Parsed _ (Right bs))
         = Set.fromList as == Set.fromList bs
sameAddr _ _ = False



-- Нечёткий поиск подстроки
--
-- Разбивает образец и проверяемую строку на слова. Далее слова образца 
-- сцепляет вместе, чтобы нормализовать пробелы. А из слов проверяемой строки 
-- сооружает множество словосечетаний, чтобы количество слов в них не превышало 
-- таковое в образце. Например для:
--     pattern = "a b c"
--     testing = "a b c d e"
-- Будут комбинации:
--     "a b c"
--     "b c d"
--     "c d e"
-- После этого применяет алгоритм Дамерау-Левенштейна образцу и каждому из 
-- словосочетаний. Возвращает минимальное из получившихся расстояний.
searchIn :: String -> String -> Int
pattern `searchIn` testing = minimum $ distance pattern' `map` testing'
   where
      distance = restrictedDamerauLevenshteinDistance defaultEditCosts
      pattern' = unwords (words pattern) -- нормализую пробелы
      testing' = flip map [0..v] $ \i -> unwords $ slice i p (words testing)
         -- Собираю возможные комбинации слов из проверяемой строки
         -- с тем же количеством слов, что и в образце.
         where p = length (words pattern)
               t = length (words testing)
               v = notNegative $ t - (p - 1) -- количество комбинаций слов
                   where notNegative n | n < 0 = 0
                                       | otherwise = n
               slice a b = take b . drop a



-- Количество адресов с парой
matchedCount :: [Parsed] -> [Parsed] -> Int
matchedCount xs ys = Set.size $ toSet xs `Set.intersection` toSet ys



-- Формирует список дубликатов (одинаковое множество компонент)
-- Возвращает количество повторений каждого из дубликатов
duplicates :: [Parsed] -> [(Parsed, Int)]
duplicates ps = [ (dup, count)
                | dups@(dup:_) <- groupBy sameAddr ps
                , let count = length dups
                , count > 1
                ]



-- Формирует список не распарсенных адресов в группе
notParsed :: [Parsed] -> [Parsed]
notParsed = filter (isLeft . parsedComps)
   where isLeft (Left _) = True
         isLeft _ = False



-- Для распарсенных адресов первой группы, которым не нашлось пары во второй 
-- группе, нахожу степень похожести на каждый из адресов второй группы.
notMatched ::
   [Parsed] -> [Parsed] ->
   [(
      Parsed,
      Either
         ErrMsg -- почему не удалось найти альтернативы
         [(
             String -- один из адресов второй группы
          ,  Int    -- степень соответствия адресу первой группы
          ,  Bool   -- есть ли уже у этого адреса пара из первой группы
         )]
   )]
notMatched xs ys =

   [ (p, alts)

   | let withPair = toSet xs `Set.intersection` toSet ys
       -- Адреса 1-й группы, которым есть пара во 2-й

   , p@(Parsed _ (Right noPairComps)) <- deleteFirstsBy sameAddr xs ys
       -- Адрес 1-й группы без пары во 2-й

   , let alts = case isRoad `find` noPairComps of
            Nothing   -> Left "В адресе нет названия дороги!"
            Just road -> Right $
               sortBy (\ (_,x,_) (_,y,_) -> y `compare` x)
                      [ (string, fit, matched)
                      | let road' = getRoad road
                            max   = maxDistance road'
                      , Parsed (Address string origin context) comps <- ys
                      , let fit = road' `icSearchIn` string
                      , fit < max
                      , let matched = either (const False)
                                             (`Set.member` withPair)
                                             comps
                      ]
   ]

   where

      icSearchIn :: String -> String -> Int
      a `icSearchIn` b = map toLower a `searchIn` map toLower b
          -- Регистронезависимый поиск подстроки

      -- Допустимое количество ошибок, при которых адреса считаются похожими.
      -- Зависит от длины шаблона.
      maxDistance :: String -> Int
      maxDistance pattern
         | l <= 2 = 0
         | l <= 4 = 1
         | l <= 6 = 2
         | otherwise = l `div` 2
         where l = length pattern
