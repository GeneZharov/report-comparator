import Data.Types
import Data.Analysis
import Address.Main (parseAddr)

toParsed = Parsed (Address "" (Photos "") "") . parseAddr
x = toParsed "Амурская ул. д. 62-А"
y = toParsed "Амурская ул. д. 62-b"

main :: IO ()
main = print (sameAddr x y)
