{-# LANGUAGE FlexibleContexts #-}
-- Включаю расширение языка. Необходимо, чтобы можно было определять сигнатуру 
-- вроде 'Stream s m Char', как в библиотеках GHC, а не просто 'Stream s m t'.


module Address.Utils where

-- TODO: Новая версия Parsecвыдаёт ошибку типов, которую я не понимаю
--import Text.Parsec
--import Text.Parsec.String (GenParser)
import Text.ParserCombinators.Parsec
import Control.Monad (liftM2)
import Data.Char (toLower, toUpper)
import Debug.Trace (trace)


-- Регистронезависимый парсер символа
--chars :: Stream s m Char => Char -> ParsecT s u m Char
chars c = char (toLower c) <|> char (toUpper c)


-- Регистронезависимый парсер строки
--strings :: Stream s m Char => String -> ParsecT s u m String
strings s = try (mapM chars s) <?> "\"" ++ s ++ "\""


manyTill1 :: GenParser tok st a
          -> GenParser tok st end
          -> GenParser tok st [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)


-- Утилита для наблюдения за процессом разбора.
-- Выводит: "оставшая не разобранная строка" — "принятый параметр"
--
-- Обычный trace, который не связан с getInput, из-за своей ленивости не 
-- покажет текст для каждой попытки разбора, так что пользуюсь этим.
watch comment = getInput >>= \rest -> trace ('"':rest ++ "\"; " ++ comment) (return ())
