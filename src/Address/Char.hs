-- Компонента адреса со значением в виде одной буквы


module Address.Char (prefix, postfix) where


import Text.Parsec
import Control.Applicative hiding (optional, (<|>), many)
import Debug.Trace (trace)
import Data.Char (toLower)

import Address.Utils
import Address.Types


prefix :: Parsec String Bool Component
prefix = do
    watch "char prefix"
    choice $ flip map keys $ \ (constr, key) ->
        let fullKey  = fst key *> skipMany1 space
            shortKey = snd key *> (char '.' *> skipMany space
                               <|> skipMany1 space)
        in do
            watch $ "char test " ++ show (constr '?')
            (try fullKey <|> try shortKey)
                *> fmap (constr . toLower) value
                <* lookAhead sep


postfix :: Parsec String Bool Component
postfix = do
    watch "char postfix"
    value <- value <* skipMany1 space
    choice $ flip map keys $ \ (constr, key) ->
        let fullKey  = fst key <* lookAhead sep
            shortKey = snd key <* (char '.' <|> lookAhead sep)
        in do
            watch $ "char test " ++ show (constr '?')
            try fullKey <|> try shortKey
            return (constr $ toLower value)


sep :: Parsec String Bool Char
sep = space
  <|> char ','
  <|> char '.' -- Бывают адреса с точкой-разделителем
  <|> eof *> return 'x'


value :: Parsec String Bool Char
value = do
    watch "value"
    letter


keys = [

        -- Например "д.117 Лит А". Имеется в виду дом №117А, но записано через 
        -- фиктивную компоненту "Литера". Так пишут таблички на домах в Питере:
        -- "20 литер А".
        ( Литера, (
            strings "литера",
            try (strings "лит") <|> strings "литер"
        ) )

    ]
