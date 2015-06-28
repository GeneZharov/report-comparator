module Main.Header where



import Paths_report_comparator
import Control.Monad
import Data.Maybe (isNothing)
import Control.Exception
import System.Process (proc)
import Data.IORef       -- для IORef
import System.IO.Unsafe -- для IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Utils.GTK
import Utils.PythonStdout
import Data.Types
import Data.Extraction
import Address.Main (parseAddr)
import Main.Body



-- Глобальное изменяемое состояние, содержит данные ныне сравниваемых адресов. 
-- Пользователь может изменить ареса в этих данных и снова запустить 
-- сопоставление.
{-# NOINLINE comparingData #-}
comparingData :: IORef ([Address], [Address])
comparingData = unsafePerformIO $ newIORef undefined



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


-- Обрабтчик клика по "Сравнить"
compareReports :: Builder -> IO ()
compareReports b = do

   photos        <- builderGetObject b castToFileChooserButton "photos"
   notes         <- builderGetObject b castToFileChooserButton "notes"
   photosDirMode <- builderGetObject b castToComboBox "photosDirMode"
   notesColumn   <- builderGetObject b castToSpinButton "notesColumn"
   notesSheets   <- builderGetObject b castToComboBox "notesSheets"

   --photosDir <- fileChooserGetFilename photos
   --notesFile <- fileChooserGetFilename notes
   --dirMode   <- liftM (== 0) (comboBoxGetActive photosDirMode)
   --colNum    <- spinButtonGetValueAsInt notesColumn
   --Just sheetName <- comboBoxGetActiveText notesSheets
   let photosDir = Just "/_reports/friso-test"
       notesFile = Just "/_reports/2014.09.15.xls"
       sheetName = "ФРИСОЛАК"
       dirMode   = False
       colNum    = 3

   if any isNothing [photosDir, notesFile]
   then do
      mainWindow <- builderGetObject b castToWindow "mainWindow"
      alert mainWindow "Заданы не все данные"
   else do
      photos <- try $ fromPhotos dirMode (getFileName photosDir)
      notes  <- try $ fromNotes sheetName (colNum - 1) (getFileName notesFile)
      case (photos, notes) of
         (Left err, _) -> report b err
         (_, Left err) -> report b err
         (Right photos', Right notes') -> do
            writeIORef comparingData (photos', notes')
            draw b (parse photos') (parse notes')
               -- `catch` \ (e :: SomeException)
               --        -> alert mainWindow (show e)

   where

      parse :: [Address] -> [Parsed]
      parse ds = [ Parsed d p
                 | d@(Address _ f _) <- ds
                 , let p = parseAddr f
                 ]

      report :: Builder -> IOError -> IO ()
      report b err = do
         mainWindow <- builderGetObject b castToWindow "mainWindow"
         alert mainWindow (show err)
