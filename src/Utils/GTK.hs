module Utils.GTK where


import Graphics.UI.Gtk
import Data.Maybe (fromJust)
import Codec.Binary.UTF8.Light (decode)
import Data.ByteString.Char8 (pack)


-- Создаёт попап с сообщением об ошибке
alert :: Window -> String -> IO ()
alert parentWin msg = do
   dialog <- messageDialogNew (Just parentWin) [] MessageError ButtonsOk msg
   set dialog [windowTitle := "Ошибка" ]
   onResponse dialog $ const (widgetHide dialog) -- сокрытие по "Ok"
   widgetShow dialog


genLabel :: String -> IO Label
genLabel text = do
    label <- labelNew Nothing
    miscSetAlignment label 0 0
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
   -- dev-haskell/gtk-0.12.4 имеет проблему кодировки при получении имени файла 
   -- из GtkFileChooserButton. Похоже, что она использует UTF-8 вместо 
   -- встроенной в хаскель юникодной кодировки. Поэтому использую специальный 
   -- хак для извлечения текста.


meta :: String -> String
meta text =
   "<span fgcolor=\"#6D6D6D\">" ++ text ++ "</span>"


italicMeta :: String -> String
italicMeta text =
   "<span fgcolor=\"#6D6D6D\" style=\"italic\">" ++ text ++ "</span>"
