module Utils.ToReadable where


import Data.Char (chr)
import Text.Regex.TDFA


-- Заменяет юникоды вида "\1077" в строках, которые создают функции вроде 
-- print/show на читаемые символы. Например: putStrLn (toReadable file)
toReadable :: String -> String
toReadable str = replace (str =~ "\\\\([0-9]{4})")
   where replace :: (String, String, String, [String]) -> String
         replace (before, [], [], []) = before
         replace (before, matched, after, groups) =
            let readable = chr $ read $ head groups
            in before ++ [ readable ] ++ toReadable after
