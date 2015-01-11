-- Цифровые компоненты адреса
--
-- Не распознаю:
-- • Номер дома без ключа, например: Волгоградский проспект, 0
-- • Слипшиеся ключ и значение:
--    1к1 — так выдаёт адреса яндекс
--    д1
--    1д — так вообще не пишут


module Address.Digit (digitComp) where


import Address.Utils
import Address.Types
import Text.Parsec
import Control.Applicative hiding (optional, (<|>))



digitComp = do
    watch "digit comp"
    try дом
        <|> try корпус
        <|> try строение
        <|>     владение

дом      = combinations Дом ключДома
корпус   = combinations Корпус ключКорпуса
строение = combinations Строение ключСтроения
владение = combinations Владение ключВладения



combinations c key = do
    watch $ "digit comb " ++ (show $ c $ HouseNum (Part 0 Nothing) Nothing)
    try (prefix c key) <|> postfix c key

prefix c key = do
    watch "digit prefix"
    (try fullKey <|> shortKey) *> fmap c number
        where fullKey  = fst key *> skipMany1 space
              shortKey = snd key *> (char '.' *> skipMany space <|> skipMany1 space)

postfix c key = do
    watch "digit postfix"
    fmap c number <* skipMany1 space <* (try fullKey <|> shortKey)
        where fullKey  = fst key
              shortKey = snd key <* optional (string ".")



number = do
    -- Полный номер, например "1A/2B"
    -- Встречаются корпуса с буквой. По тем же соображениям, что и номера 
    -- домов, номера корпусов и строений также могут быть с буквой и через 
    -- слэш.
    watch "number"
    optional (char '№') *> (HouseNum <$> part <*> option Nothing suffix)
        where suffix = (char '/' <|> char '-') *> fmap Just part
           -- Разделитель дефисом — это костыль в здравпросвете,
           -- так как в имени файла не может быть слэша.

part = do
    -- Половина номера с левой или правой стороны от слэша.
    -- Например "2-B" или "2B"
    watch "part"
    Part
        <$> fmap (read :: String -> Int) (many1 digit)
        <*> option Nothing (try $ optional (char '-') *> fmap Just letter)



ключДома = (
        strings "дом",
        strings "д"
    )

ключКорпуса = (
        strings "корпус",
        try (strings "корп") <|> try (strings "кор") <|> strings "к"
    )

ключСтроения = (
        strings "строение",
        try (strings "стр") <|> strings "с"
    )

ключВладения = (
        strings "владение",
        strings "вл"
    )
