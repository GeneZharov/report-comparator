module Main.Header where



import Paths_report_comparator
import Control.Monad
import Control.Exception
import System.Process (proc)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Utils.GTK
import Utils.PythonStdout
import Data.Extraction



-- Обработчик выбора файла с табличным отчётом, обновляет меню страниц таблицы
updateSheets :: Builder -> IO ()
updateSheets b = do

   py   <- getDataFileName "tables/sheet-names"
   file <- builderGetObject b castToFileChooserButton "notes"
       >>= liftM getFileName . fileChooserGetFilename
   try (pythonStdout (proc "python" [py, file])) >>= update file

   where
      update :: String -> Either IOError String -> IO ()

      update file (Left parseErr) = do

         -- Очищаю меню табличного отчёта
         mainWindow <- builderGetObject b castToWindow "mainWindow"
         notes <- builderGetObject b castToFileChooserButton "notes"
         fileChooserUnselectAll notes

         -- Очищаю меню страниц табличного отчёта
         notesSheets <- builderGetObject b castToComboBox "notesSheets"
         comboBoxSetModelText notesSheets

         alert mainWindow $
            "Не удалось открыть электронную таблицу:\n" ++ file
            ++ "\n\n" ++ show parseErr

      update _ (Right sheetNames) = do

         -- Наполняю combobox с названиями страниц новым содержимым
         notesSheets <- builderGetObject b castToComboBox "notesSheets"
         comboBoxSetModelText notesSheets
         forM_ (lines sheetNames)
             $ \ sheet -> comboBoxAppendText notesSheets sheet

         -- Первый пункт меню — активный
         set notesSheets [comboBoxActive := 0]



updatePhotosPreview :: Builder -> IO ()
updatePhotosPreview b = do

   photos         <- builderGetObject b castToFileChooserButton "photos"
   photosPreview  <- builderGetObject b castToImage "photosPreview"

   Just photosDir <- fileChooserGetFilename photos
   tooltip        <- genPhotosPreview photosDir
   set photosPreview [widgetTooltipText := Just tooltip]



updateNotesPreview :: Builder -> IO ()
updateNotesPreview b = do

   notes        <- builderGetObject b castToFileChooserButton "notes"
   notesSheets  <- builderGetObject b castToComboBox "notesSheets"
   notesPreview <- builderGetObject b castToImage "notesPreview"

   Just notesFile <- fileChooserGetFilename notes
   Just sheetName <- comboBoxGetActiveText notesSheets

   tooltip <- genNotesPreview sheetName notesFile
   set notesPreview [widgetTooltipText := Just tooltip]
