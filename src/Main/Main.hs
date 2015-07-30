import Paths_report_comparator
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Control.Monad (forM_)

import Main.Header
import Main.Body
import Main.EditAll
import Data.Types



-- Настройки, которые не удаётся сделать через glade
prepareGUI :: Builder -> IO ()
prepareGUI b = do

   -- Цвет фона для вкладок
   let c = 62000 -- 65535 — максимум
   forM_ [ "photosVP", "notesVP", "matchedVP" ]
       $ \ id -> do
            vp <- builderGetObject b castToViewport id
            -- TODO: В Windows фон по умолчанию ярче, поэтому цвет получается 
            -- похожим на дефолтный. Пытался считать дефолтный, чтобы увеличить 
            -- его на константу, но почему-то в Windows и Unix цвет считывается 
            -- одинаковый, хотя визуально они явно различаются.
            --style <- get vp widgetStyle
            --print =<< styleGetBackground style StateNormal
            widgetModifyBg vp StateNormal (Color c c c)

   -- Dirmode по умолчанию
   photosDirMode <- builderGetObject b castToComboBox "photosDirMode"
   comboBoxSetActive photosDirMode 0



setupHandlers :: Builder -> IO ()
setupHandlers b = do

   -- Обновление меню страниц таблицы
   notes <- builderGetObject b castToFileChooserButton "notes"
   after notes currentFolderChanged (updateSheets b)

   -- Обновление preview
   photos      <- builderGetObject b castToFileChooserButton "photos"
   notesSheets <- builderGetObject b castToComboBox "notesSheets"
   after photos currentFolderChanged (updatePhotosPreview b)
   after notesSheets changed (updateNotesPreview b)

   -- Клик по "editAll"
   editAllPhotos <- builderGetObject b castToButton "editAllPhotos"
   on editAllPhotos buttonActivated (editAll b Photo)

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
   --editAll b Photo -- FIXME: временно для разработки
   mainGUI
