-- Библиотека для разбора CSV-файла

{- Пример использования:
 -
 - Можно вызывать parseCSV передавая внутрь файл в виде строки, либо напрямую 
 - использовать парсер csv:
 -
 - main =
 -     do c <- getContents
 -        case parse csv "(stdin)" c of
 -             Left e -> do putStrLn "Error parsing input:"
 -                          print e
 -             Right r -> mapM_ print r
 -
 -}


module ParseCSV where
import Text.Parsec


csv  = endBy line eol
line = sepBy cell (char ';')
cell = quotedCell <|> many (noneOf ";\n\r")

quotedCell =
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csv ""
