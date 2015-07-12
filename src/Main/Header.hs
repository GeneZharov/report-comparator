module Main.Header where



import Paths_report_comparator
import Control.Monad
import Control.Exception
import System.Process (proc)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import System.IO.Error (catchIOError)
import Data.Maybe (fromJust)

import Utils.GTK
import Utils.PythonStdout
import Data.Extraction



-- Обработчик выбора файла с табличным отчётом, обновляет меню страниц таблицы
updateSheets :: Builder -> IO ()
updateSheets b = do

   file <- builderGetObject b castToFileChooserButton "notes"
       >>= liftM (decodeFileName . fromJust) . fileChooserGetFilename

   f <- fileChooserGetFilename
      =<< builderGetObject b castToFileChooserButton "notes"
   case f of
      Nothing -> clearMenu b -- в windows меню становится пустым при закрытии dialog window
      Just f' -> do
         let f'' = decodeFileName f'
         py <- getDataFileName "tables/sheet-names"
         update f'' =<< try ( pythonStdout (proc "python" [py, f'']) )

   where
      update :: String -> Either IOError String -> IO ()

      update file (Left parseErr) =
         report b $ "Не удалось открыть электронную таблицу:\n" ++ file
                 ++ "\n\n" ++ show parseErr

      update _ (Right []) =
         report b "В таблице не найдено ни одной страницы"

      update _ (Right sheetNames) = do
         -- Наполняю combobox с названиями страниц новым содержимым
         notesSheets <- builderGetObject b castToComboBox "notesSheets"
         comboBoxSetModelText notesSheets
         forM_ (lines sheetNames)
             $ \ sheet -> comboBoxAppendText notesSheets sheet
         -- Первый пункт меню — активный
         set notesSheets [comboBoxActive := 0]

      report :: Builder -> String -> IO ()
      report b msg = clearMenu b >> alert b msg

      clearMenu :: Builder -> IO ()
      clearMenu b = do
         -- Очищаю меню табличного отчёта
         mainWindow <- builderGetObject b castToWindow "mainWindow"
         notes <- builderGetObject b castToFileChooserButton "notes"
         fileChooserUnselectAll notes
         -- Очищаю меню страниц табличного отчёта
         notesSheets <- builderGetObject b castToComboBox "notesSheets"
         comboBoxSetModelText notesSheets >> return ()



updatePhotosPreview :: Builder -> IO ()
updatePhotosPreview b = do

   photos        <- builderGetObject b castToFileChooserButton "photos"
   photosPreview <- builderGetObject b castToImage "photosPreview"

   photosDir <- (decodeFileName . fromJust)
                `liftM` fileChooserGetFilename photos
   tooltip   <- genPhotosPreview photosDir

   set photosPreview [widgetTooltipText := Just tooltip]
      `catchIOError` \ err -> alert b (show err)



updateNotesPreview :: Builder -> IO ()
updateNotesPreview b = do

   notes        <- builderGetObject b castToFileChooserButton "notes"
   notesSheets  <- builderGetObject b castToComboBox "notesSheets"
   notesPreview <- builderGetObject b castToImage "notesPreview"

   notesFile    <- (decodeFileName . fromJust)
                   `liftM` fileChooserGetFilename notes
   sheetName    <- comboBoxGetActiveText notesSheets

   case sheetName of
      Nothing -> return ()
      Just sheetName' -> do
         tooltip <- genNotesPreview sheetName' notesFile
         set notesPreview [widgetTooltipText := Just tooltip]
            `catchIOError` \ err -> alert b (show err)
