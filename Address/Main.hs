module Address.Main where


import Address.Types
import Address.Digit
import Address.Symbol
import Text.Parsec
import Control.Applicative hiding (optional, (<|>), many)
import Debug.Trace (trace)


address = many space *>
          component `sepBy` (optional (char ',') *> many space)
          <* many space


component = do
    --getInput >>= (`trace` (return ()))
    try digitComp <|> symbolComp


parseAddr = parse address ""
