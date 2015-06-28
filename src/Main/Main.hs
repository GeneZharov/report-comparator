import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Data.Maybe (isNothing, fromJust)
import Data.List (sortBy, isInfixOf)
import Text.Parsec.Error (ParseError)
import Control.Exception
import System.IO.Error (ioeGetFileName)
import Data.Ord (comparing)
import System.Process (proc)
import Debug.Trace

-- Для IORef
import Data.IORef
import System.IO.Unsafe

import Paths_report_comparator
import Main.Utils
import Data.Types
import Data.Extraction (pythonStdout, fromNotes, fromPhotos)
import Data.Analysis (matchedCount, duplicates, notParsed, notMatched)
import Address.Types
import Address.Main (parseAddr)



-- Глобальное изменяемое состояние, содержит данные ныне сравниваемых адресов. 
-- Пользователь может изменить ареса в этих данных и снова запустить 
-- сопоставление.
{-# NOINLINE comparingData #-}
comparingData :: IORef ([Address], [Address])
comparingData = unsafePerformIO $ newIORef undefined



-- Обработчик выбора файла с табличным отчётом, обновляет меню страниц таблицы
updateSheets :: Builder -> IO ()
updateSheets b = do

   py <- getDataFileName "tables/sheet-names"
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
             $ \sheet -> comboBoxAppendText notesSheets sheet

         -- Первый пункт меню — активный
         set notesSheets [comboBoxActive := 0]



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
   --sheetName <- liftM fromJust (comboBoxGetActiveText notesSheets)
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



-- Отрисовывает результат сопоставления отчётов
draw :: Builder -> [Parsed] -> [Parsed] -> IO ()
draw b photos notes = do

   let bothMatched      = matchedCount photos notes
       photosDuplicates = duplicates   photos
       photosNotParsed  = notParsed    photos
       photosNotMatched = notMatched   photos notes
       notesDuplicates  = duplicates   notes
       notesNotParsed   = notParsed    notes
       notesNotMatched  = notMatched   notes  photos

   -- Количество адресов каждого типа
   drawStat b "photosStat"
      (length photos)
      bothMatched
      (length photosDuplicates)
      (length photosNotParsed)
      (length photosNotMatched)
   drawStat b "notesStat"
      (length notes)
      bothMatched
      (length notesDuplicates)
      (length notesNotParsed)
      (length notesNotMatched)

   -- Адреса, которые не удалось распарсить
   drawNotParsed b "photosNotParsed" photosNotParsed
   drawNotParsed b "notesNotParsed"  notesNotParsed
   --print photosNotParsed
   --print notesNotParsed

   -- Дубликаты
   drawDuplicates b "photosDuplicates" photosDuplicates
   drawDuplicates b "notesDuplicates"  notesDuplicates

   -- Адреса без пары
   drawNotMatched b "photosNotMatched" photosNotMatched
   drawNotMatched b "notesNotMatched"  notesNotMatched
   --print notesNotMatched
   --print photosNotMatched

   when (bothMatched == 0) $ do
      mainWindow <- builderGetObject b castToWindow "mainWindow"
      alert mainWindow
         "Нет ни одного совпадения адресов.\n\n\
         \Возможно отчёты полностью отличаются \
         \или из них неверно извлечены адреса:\n\n\
         \• Возможно адреса в таблице лежат в другой колонке;\n\n\
         \• Возможно надо воспользоваться переключателем типа файлов, из \
         \которых извлекаются адреса фотоотчёта;"



drawStat :: Builder -> String -> Int -> Int -> Int -> Int -> Int -> IO ()
drawStat b containerID total matched duplicates notParsed notMatched =
   let stat = zip [0..]
            . filter ( (>0) . fst )
            . sortBy (flip (comparing fst))
            $ [
                  ( total      , "— всего" )
              ,   ( matched    , "— есть соответствия" )
              ,   ( duplicates , "— дубликаты" )
              ,   ( notParsed  , "— не поддаются анализу" )
              ,   ( notMatched , "— нет соответствий" )
              ]
   in do
      table <- builderGetObject b castToTable containerID
      destroyChildren table
      forM_ stat $ \(i, (num, text)) -> do
         genLabel (show num) >>= addCell table 0 i
         genLabel text       >>= addCell table 1 i
      widgetShowAll table



drawMatched :: Builder -> String -> Int -> IO ()
drawMatched b labelID count = do
   label <- builderGetObject b castToLabel labelID
   labelSetMarkup label (show count)



-- Отрисовывает дубликаты адресов.
-- Принимает набор данных и id виджета, в который вставлять результат.
drawDuplicates :: Builder -> String -> [(Int, Parsed)] -> IO ()
drawDuplicates b containerID model = do

   -- Создаю таблицу
   table <- tableNew (length model) 2 False
      -- Количество строк, столбцов, homogeneous
   tableSetRowSpacings table 7
   tableSetColSpacing table 0 7 -- номер колонки, количество пикселей

   -- Наполняю таблицу строками
   if null model
   then genLabel (italicMeta "Пусто") >>= addCell table 0 0
   else forM_ (zip [0..] model) $
      \ ( dupNum
        ,  ( dupsCount
           , Parsed (Address real _ _) (Right comps)
           )
        ) -> do

           -- Ячейка с текстом адреса
           srcLabel <- genLabel real
           set srcLabel [
               widgetTooltipText := Just (format comps)
             , labelSelectable := True
             ]
           addCell table 0 dupNum srcLabel

           -- Ячейка с количеством дубликатов
           genLabel ("— " ++ show dupsCount ++ " шт.")
              >>= addCell table 1 dupNum

   -- Подставляю таблицу в контейнер и показываю результат
   alignment <- builderGetObject b castToAlignment containerID
   destroyChildren alignment
   containerAdd alignment table
   widgetShowAll table



-- Отрисовывает не распарсенные адреса.
-- Принимает набор данных и id виджета, в который вставлять результат.
drawNotParsed :: Builder -> String -> [Parsed] -> IO ()
drawNotParsed b containerID model = do

   -- Создаю таблицу
   table <- tableNew (length model) 2 False
       -- Количество строк, столбцов, homogeneous
   tableSetRowSpacings table 7

   -- Наполняю таблицу строками
   if null model
   then genLabel (italicMeta "Пусто") >>= addCell table 0 0
   else forM_ (zip [0..] model) $ \ (i, Parsed (Address real _ _) _) -> do
           label <- genLabel real
           labelSetSelectable label True
           addCell table 0 i label
           --genLabel "Error not implemented" >>= addCell table 1 i

   -- Подставляю таблицу в контейнер и показываю результат
   alignment <- builderGetObject b castToAlignment containerID
   destroyChildren alignment
   containerAdd alignment table
   widgetShowAll table



-- Отрисовывает адреса без пары. Принимает набор данных и идентификатор 
-- виджета, в который вставлять результат.
drawNotMatched :: Builder -> String
               -> [(
                     Parsed,
                     Either ErrMsg [(String, Int, Bool)]
                  )]
               -> IO ()
drawNotMatched b containerID model = do

   -- Создаю таблицу для адресов без пар
   table <- tableNew (length model) 2 False -- строк, столбцов, homogeneous
   tableSetRowSpacings table 7
   tableSetColSpacings table 40

   genLabel (meta "Без пары из текущей группы адресов") >>= addCell table 0 0
   genLabel (meta "Похожие из другой группы адресов"  ) >>= addCell table 1 0

   -- Генерю строки с адресами, которым не нашлось пары
   if null model
   then genLabel (italicMeta "Пусто") >>= addCell table 0 0
   else forM_ (zip [1..] model) $
      \ ( i
        ,  ( Parsed (Address real _ _) (Right comps)
           , options
           )
        ) -> do

           -- Линия-разделитель
           separator <- hSeparatorNew
           tableAttach table separator 0 2 (2*i) (2*i+1) [Fill] [] 0 0

           -- Строка адреса без пары
           leftLabel <- genLabel real
           set leftLabel [
               widgetTooltipText := Just (format comps)
             , labelSelectable := True
             ]
           --labelSetSelectable leftLabel True
           addCell table 0 (i*2+1) leftLabel

           -- Похожие адреса
           case options of
              Left err -> do
                 errorLabel <- genLabel (italicMeta err)
                 labelSetSelectable errorLabel True
                 addCell table 1 (i*2+1) errorLabel
              Right options' -> do
                 vbox <- vBoxNew True 7 -- homogeneous, spacing
                 forM_ options' $ \ (addr, fit, matched) -> do
                    -- Создаю одну из альтернатив
                    hbox <- hBoxNew False 7
                    boxSetHomogeneous hbox False
                    alt <- genLabel addr
                    labelSetSelectable alt True
                    boxPackStart hbox alt PackNatural 0
                    when matched $ do
                        pairedLabel <- genLabel (italicMeta "— уже имеет пару")
                        boxPackStart hbox pairedLabel PackNatural 0
                    boxPackEndDefaults vbox hbox -- Добавляю hbox в конец vbox
                 addCell table 1 (i*2+1) vbox

   -- Подставляю таблицу в контейнер и показываю результат
   alignment <- builderGetObject b castToAlignment containerID
   destroyChildren alignment
   containerAdd alignment table
   widgetShowAll table

   where isLeft (Left _)     = True
         isLeft (Right _)    = False
         fromLeft (Left x)   = x
         fromRight (Right x) = x



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

   -- Выбор файла с табличным отчётом обновляет меню страниц
   notes <- builderGetObject b castToFileChooserButton "notes"
   afterCurrentFolderChanged notes (updateSheets b)

   -- Клик по "Сравнить"
   submit <- builderGetObject b castToButton "submit"
   on submit buttonActivated (compareReports b)

   widgetShowAll mainWindow
   mainGUI
