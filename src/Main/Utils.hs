module Main.Utils where


import Graphics.UI.Gtk
import Data.Char (chr)
import Text.Regex.TDFA
import Data.ByteString.Char8 (pack)
import Codec.Binary.UTF8.Light (decode)
import Data.Maybe (fromJust)

import Address.Types


-- Создаёт попап с сообщением об ошибке
alert :: Window -> String -> IO ()
alert parentWin msg = do
    dialog <- messageDialogNew (Just parentWin) [] MessageError ButtonsOk msg
    set dialog [windowTitle := "Ошибка" ]
    onResponse dialog $ const (widgetHide dialog) -- сокрытие по "Ok"
    widgetShow dialog


-- Заменяет юникоды вида "\1077" в строках, которые создают функции вроде 
-- print/show на читаемые символы.
toReadable :: String -> String
toReadable str = replace ( str =~ "\\\\([0-9]{4})" )
    where replace :: (String, String, String, [String]) -> String
          replace (before, [], [], []) = before
          replace (before, matched, after, groups) =
              let readable = chr $ read $ head groups
              in before ++ [ readable ] ++ toReadable after


-- Приводит набор компонент адреса к читаемой строке
format :: [Component] -> String
format = init . tail     -- Обрезаю фигурные скобки
       . map newlines . toReadable . show
    where newlines c | c == ',' = '\n'
          newlines c = c
          -- Заменяет запятую на перенос строки


genLabel :: String -> IO Label
genLabel text = do
    label <- labelNew Nothing
    miscSetAlignment label  0 0
    labelSetMarkup label text
    return label


addCell table x y widget =
    tableAttach table widget
        x (x+1)       -- колонка слева/справа
        y (y+1)       -- строка сверху/снизу
        [Fill] [Fill] -- horizontal/vertical resizing
        0 0           -- padding горизонтальный/вертикальный


-- Удаляет содержимое контейнера
destroyChildren container =
    containerGetChildren container >>= mapM_ widgetDestroy



getFileName :: Maybe FilePath -> String
getFileName = decode . pack . fromJust
    -- dev-haskell/gtk-0.12.4 имеет проблему кодировки при получении имени 
    -- файла из GtkFileChooserButton. Похоже, что она использует UTF-8 вместо 
    -- встроенной в хаскель юникодной кодировки. Поэтому использую специальный 
    -- хак для извлечения текста.



meta text =
    "<span fgcolor=\"#6D6D6D\" \
    \>" ++ text ++ "</span>"


italicMeta text =
    "<span fgcolor=\"#6D6D6D\" style=\"italic\" \
    \>" ++ text ++ "</span>"
