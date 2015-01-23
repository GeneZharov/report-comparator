-- Символьные компоненты адреса
--
-- Не распознаю:
-- • Смежные символьные компоненты, если их значения слиплись и не разделены 
--    запятой или ключами. Например: "г. Москва 1-я Дубровская ул". Тут 
--    программно просто невозможно различить границу между значениями 
--    компонент, если не иметь словаря городов/улиц.


module Address.Symbol (prefix, postfix) where


import Address.Utils
import Address.Types
import qualified Address.Digit as D
import Text.Parsec
import Control.Applicative hiding (optional, (<|>), many)
import Debug.Trace (trace)


prefix = do
    watch "symbol prefix"
    choice $ flip map keys $ \ (constr, key) ->
        let fullKey  = fst key *> skipMany1 space
            shortKey = snd key *> (char '.' *> skipMany space
                               <|> skipMany1 space)
        in do
            watch $ "symbol test " ++ show (constr "")
            (try fullKey <|> try shortKey)
                *> fmap constr value
                <* lookAhead sep


postfix = do
    watch "symbol postfix"
    value <- value <* skipMany1 space
    choice $ flip map keys $ \ (constr, key) ->
        let fullKey  = fst key <* lookAhead sep
            shortKey = snd key <* (char '.' <|> lookAhead sep)
        in do
            watch $ "symbol test " ++ show (constr "")
            try fullKey <|> try shortKey
            return (constr value)


sep = space
  <|> char ','
  <|> char '.' -- Бывают адреса с точкой-разделителем
  <|> eof *> return 'x'


value = do
    watch "value"
    manyTill1 anyChar $ lookAhead $
            try (many  space <* eof)
        <|> try (many  space <* char ',')
        <|> try (many1 space <* choice (map symbolKey keys))
        <|> try (many  space <* (try D.prefix <|> try D.postfix))
    where symbolKey (constr, key) = do
              watch $ "symbolKey " ++ show (constr "")
              try (fst key <* sep) <|> try (snd key <* (sep <|> char '.'))
          sep = space
            <|> char ','
            <|> eof *> return 'x'


keys = [

        ( Город, (
            strings "город",
            try (strings "гор") <|> strings "г"
        ) ),

        ( Посёлок, (
            strings "пос" *> oneOf "ёе" *> strings "лок",
            try (strings "пос") <|> strings "п"
        ) ),

        ( Село, (
            strings "село",
            many1 (satisfy (const False))
                -- Сокращение 'с' конфликтует со строением
        ) ),

        ( Деревня, (
            strings "деревня",
            many1 (satisfy (const False))
        ) ),

        ( Микрорайон, (
            strings "микрорайон",
            strings "мкр"
        ) ),

        ( Улица, (
            strings "улица",
            strings "ул"
        ) ),

        ( Шоссе, (
            strings "шоссе",
            strings "ш"
        ) ),

        ( Переулок, (
            strings "переулок",
            strings "пер"
        ) ),

        ( Бульвар, (
            strings "бульвар",
            strings "б-р"
        ) ),

        ( Проспект, (
            strings "проспект",
            try (strings "пр-т") <|> strings "пр"
        ) ),

        ( Набережная, (
            strings "набережная",
            strings "наб"
        ) )

    ]
