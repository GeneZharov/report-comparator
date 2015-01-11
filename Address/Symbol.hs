-- Символьные компоненты адреса
--
-- Не распознаю:
-- • Смежные символьные компоненты, если их значения слиплись и не разделены 
--    запятой или ключами. Например: "г. Москва 1-я Дубровская ул". Тут 
--    программно просто невозможно различить границу между значениями 
--    компонент.


module Address.Symbol (symbolComp) where


import Address.Utils
import Address.Types
import Address.Digit
import Text.Parsec
import Control.Applicative hiding (optional, (<|>), many)
import Debug.Trace (trace)


symbolComp = do
    watch "symbol comp"
    try город
        <|> try посёлок
        <|> try село
        <|> try деревня
        <|> try микрорайон
        <|> try улица
        <|> try шоссе
        <|> try переулок
        <|> try бульвар
        <|> try проспект
        <|>     набережная


город      = combinations Город ключГорода
посёлок    = combinations Посёлок ключПосёлка
село       = combinations Село ключСела
деревня    = combinations Деревня ключДеревни
микрорайон = combinations Микрорайон ключМикрорайона
улица      = combinations Улица ключУлицы
шоссе      = combinations Шоссе ключШоссе
переулок   = combinations Переулок ключПереулка
бульвар    = combinations Бульвар ключБульвара
проспект   = combinations Проспект ключПроспекта
набережная = combinations Набережная ключНабережной


combinations c key = do
    watch $ "symbol comb " ++ (show $ c "")
    try (prefix c key) <|> postfix c key

prefix c key = do
    watch "symbol prefix"
    (try fullKey <|> shortKey) *> fmap c value
        where fullKey  = fst key *> skipMany1 space
              shortKey = snd key *> (char '.' *> skipMany space <|> skipMany1 space)

postfix c key = do
    watch "symbol postfix"
    fmap c value <* skipMany1 space <* (try fullKey <|> shortKey)
        where fullKey  = fst key
              shortKey = snd key <* optional (string ".")


--value = manyTill1 anyChar (trace "Symbol Value lookAhead" $ lookAhead eof)
value = do
    watch "value"
    manyTill1 anyChar $ lookAhead $
            try (many space <* eof)
        <|> try (many space <* char ',')
        <|> try (many1 space <* symbolKey)
        <|> try (many space <* digitComp)
    where symbolKey = try (keys ключГорода)
                  <|> try (keys ключПосёлка)
                  <|> try (keys ключСела)
                  <|> try (keys ключДеревни)
                  <|> try (keys ключМикрорайона)
                  <|> try (keys ключУлицы)
                  <|> try (keys ключШоссе)
                  <|> try (keys ключПереулка)
                  <|> try (keys ключБульвара)
                  <|> try (keys ключПроспекта)
                  <|>      keys ключНабережной
          keys k = try (fst k <* sep)
                   <|> (snd k <* (sep <|> char '.'))
          sep = space
            <|> char ','
            <|> eof *> return 'x'


ключГорода = (
        strings "город",
        try (strings "гор") <|> strings "г"
    )

ключПосёлка = (
        strings "пос" *> oneOf "ёе" *> strings "лок",
        try (strings "пос") <|> strings "п"
    )

ключСела = (
        strings "село",
        many1 (satisfy (const False))
            -- Сокращение 'с' конфликтует со строением
    )

ключДеревни = (
        strings "деревня",
        many1 (satisfy (const False))
    )

ключМикрорайона = (
        strings "микрорайон",
        strings "мкр"
    )

ключУлицы = (
        strings "улица",
        strings "ул"
    )

ключШоссе = (
        strings "шоссе",
        strings "ш"
    )

ключПереулка = (
        strings "переулок",
        strings "пер"
    )

ключБульвара = (
        strings "бульвар",
        strings "б-р"
    )

ключПроспекта = (
        strings "проспект",
        try (strings "пр-т") <|> strings "пр"
    )

ключНабережной = (
        strings "набережная",
        strings "наб"
    )
