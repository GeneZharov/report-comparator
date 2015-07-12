module Utils.GTK where


import Graphics.UI.Gtk
import Data.Maybe (fromJust)
import Codec.Binary.UTF8.Light (decode)
import Data.ByteString.Char8 (pack)


-- Создаёт попап в главном окне с сообщением об ошибке
alert :: Builder -> String -> IO ()
alert b msg = do
   mainWindow <- builderGetObject b castToWindow "mainWindow"
   dialog     <- messageDialogNew (Just mainWindow) []
                                  MessageError ButtonsOk msg
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


decodeFileName :: FilePath -> String
decodeFileName = decode . pack
   -- dev-haskell/gtk-0.12.4 имеет проблему кодировки при получении имени файла 
   -- из GtkFileChooserButton. Похоже, что она использует UTF-8 вместо 
   -- встроенной в хаскель юникодной кодировки. Поэтому использую хак для 
   -- извлечения текста.


meta :: String -> String
meta text =
   "<span fgcolor=\"#6D6D6D\">" ++ text ++ "</span>"


italicMeta :: String -> String
italicMeta text =
   "<span fgcolor=\"#6D6D6D\" style=\"italic\">" ++ text ++ "</span>"
