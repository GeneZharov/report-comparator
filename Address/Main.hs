module Address.Main where


import Text.Parsec
import Control.Applicative hiding (optional, (<|>), many)
import Debug.Trace (trace)

import Address.Types
import qualified Address.Digit as D
import qualified Address.Symbol as S


parseAddr = parse address ""


address = many space *> component `sepEndBy` sep <* eof
    where sep = optional (char ',' <|> char '.') *> many space


component = try D.prefix
        <|> try S.prefix
        <|> try D.postfix
        <|>     S.postfix
