import Paths_report_comparator
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Main.Header
import Main.Body


main :: IO ()
main = do

   initGUI
   b <- builderNew
   getDataFileName "main.glade" >>= builderAddFromFile b
   mainWindow <- builderGetObject b castToWindow "mainWindow"
   windowMaximize mainWindow
   onDestroy mainWindow mainQuit

   -- Атрибут, который не удаётся задать через glade
   photosDirMode <- builderGetObject b castToComboBox "photosDirMode"
   comboBoxSetActive photosDirMode 0

   -- Выбор файла с табличным отчётом обновляет меню страниц таблицы
   notes <- builderGetObject b castToFileChooserButton "notes"
   afterCurrentFolderChanged notes (updateSheets b)

   -- Обновление preview
   photos      <- builderGetObject b castToFileChooserButton "photos"
   notesSheets <- builderGetObject b castToComboBox "notesSheets"
   --afterCurrentFolderChanged photos (updatePhotosPreview b)
   --after notesSheets changed (updateNotesPreview b)

   -- Клик по "Сравнить"
   submit <- builderGetObject b castToButton "submit"
   on submit buttonActivated (compareReports b)

   widgetShowAll mainWindow
   mainGUI
