import Paths_report_comparator
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Control.Monad (forM_)

import Main.Header
import Main.Body



-- Настройки, которые не удаётся сделать через glade
prepareGUI :: Builder -> IO ()
prepareGUI b = do

   -- Цвет фона для вкладок
   forM_ [ "photosVP", "notesVP", "matchedVP" ]
       $ \ id -> do
            let c = 62000 -- 65535 — максимум
            vp <- builderGetObject b castToViewport id
            widgetModifyBg vp StateNormal (Color c c c)

   -- Dirmode по умолчанию
   photosDirMode <- builderGetObject b castToComboBox "photosDirMode"
   comboBoxSetActive photosDirMode 0



setupHandlers :: Builder -> IO ()
setupHandlers b = do

   -- Обновление меню страниц таблицы
   notes <- builderGetObject b castToFileChooserButton "notes"
   afterCurrentFolderChanged notes (updateSheets b)

   -- Обновление preview
   photos      <- builderGetObject b castToFileChooserButton "photos"
   notesSheets <- builderGetObject b castToComboBox "notesSheets"
   afterCurrentFolderChanged photos (updatePhotosPreview b)
   after notesSheets changed (updateNotesPreview b)

   -- Клик по "Сравнить"
   submit <- builderGetObject b castToButton "submit"
   on submit buttonActivated (compareReports b) >> return ()



main :: IO ()
main = do

   initGUI
   b <- builderNew
   getDataFileName "main.glade" >>= builderAddFromFile b
   mainWindow <- builderGetObject b castToWindow "mainWindow"
   windowMaximize mainWindow
   onDestroy mainWindow mainQuit

   prepareGUI b
   setupHandlers b

   widgetShow mainWindow
   mainGUI
