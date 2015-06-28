module Utils.Addr where


import Address.Types
import Utils.ToReadable


-- Приводит набор компонент адреса к читаемой строке
format :: [Component] -> String
format = init . tail -- обрезаю фигурные скобки
       . map newlines . toReadable . show
    where newlines c | c == ',' = '\n'
          newlines c = c
          -- Заменяет запятую на перенос строки
