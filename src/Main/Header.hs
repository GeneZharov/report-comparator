module Main.Header where



import Paths_report_comparator
import Control.Monad
import Control.Exception
import System.Process (proc)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import System.IO.Error (catchIOError)
import Data.Maybe (fromJust, isNothing)
import Data.Char (toLower)

import Address.Main (parseAddr)
import Utils.GTK
import Utils.PythonStdout
import Data.Types
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



obtainPhotos :: Builder -> IO (Maybe [Parsed])
obtainPhotos b = do

   photos        <- builderGetObject b castToFileChooserButton "photos"
   photosDirMode <- builderGetObject b castToComboBox "photosDirMode"

   --photosDir <- fileChooserGetFilename photos
   let photosDir = Just undefined
   dirMode   <- (== 1) `liftM` comboBoxGetActive photosDirMode

   if isNothing photosDir
   then do
      alert b "Не задан каталог с фотографиями"
      return Nothing
   else do
      --let photosDir' = decodeFileName (fromJust photosDir)
      let photosDir'  = "/root/r/t/zd/aqua"
      photos <- try $ fromPhotos dirMode photosDir'
      case photos of
         Left err      -> report b err >> return Nothing
         Right photos' -> return $ Just (parse photos')



obtainNotes :: Builder -> IO (Maybe [Parsed])
obtainNotes b = do

   notes       <- builderGetObject b castToFileChooserButton "notes"
   notesColumn <- builderGetObject b castToSpinButton "notesColumn"
   notesSheets <- builderGetObject b castToComboBox "notesSheets"

   --notesFile <- fileChooserGetFilename notes
   --sheetName <- comboBoxGetActiveText notesSheets
   --colNum    <- spinButtonGetValueAsInt notesColumn
   let sheetName = Just undefined
       notesFile = Just undefined

   if any isNothing [notesFile, sheetName]
   then do
      alert b "Не задана таблица с отчётом или имя страницы в ней"
      return Nothing
   else do
      --let sheetName' = fromJust sheetName
      --    notesFile' = decodeFileName (fromJust notesFile)
      let sheetName' = "Лист1"
          notesFile' = "/root/r/t/zd/aqua.xlsx"
          colNum     = 3
      notes <- try $ fromNotes sheetName' (colNum-1) notesFile'
      case notes of
         Left err     -> report b err >> return Nothing
         Right notes' -> return $ Just (parse notes')



parse :: [Address] -> [Parsed]
parse as = [ Parsed a c
           | a@(Address s _ _) <- as
           , let c = parseAddr (toLower `map` s)
           ]

report :: Builder -> IOError -> IO ()
report b err = alert b (show err)
