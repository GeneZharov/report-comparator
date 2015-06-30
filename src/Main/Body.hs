module Main.Body where



import Data.Maybe (isNothing)
import Control.Exception
import Data.List (sortBy)
import Data.Ord (comparing)
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk
import Control.Monad
import System.FilePath (replaceBaseName)
import System.Directory (renameFile, removeFile)

import Address.Main (parseAddr)
import Utils.Addr
import Utils.GTK
import Data.Types
import Data.Extraction
import Data.Analysis (matchedCount, duplicates, notParsed, notMatched)



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
       colNum    = 4

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
            draw b (parse photos') (parse notes')
               -- `catch` \ (e :: SomeException)
               --        -> alert mainWindow (show e)

   where

      parse :: [Address] -> [Parsed]
      parse as = [ Parsed a c
                 | a@(Address s _ _) <- as
                 , let c = parseAddr s
                 ]

      report :: Builder -> IOError -> IO ()
      report b err = do
         mainWindow <- builderGetObject b castToWindow "mainWindow"
         alert mainWindow (show err)



-- Отрисовывает результат сопоставления отчётов
draw :: Builder -> [Parsed] -> [Parsed] -> IO ()
draw b photos notes = do

   let bothMatched      = matchedCount photos notes
       photosNotParsed  = notParsed    photos
       notesNotParsed   = notParsed    notes
       photosDuplicates = duplicates   photos
       notesDuplicates  = duplicates   notes
       photosNotMatched = notMatched   photos notes
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



-- Отрисовывает не распарсенные адреса.
-- Принимает набор данных и id виджета, в который вставлять результат.
drawNotParsed :: Builder -> String -> [Parsed] -> IO ()
drawNotParsed b containerID model = do

   -- Создание таблицы
   table <- tableNew (length model) 3 False -- строк, столбцов, homogeneous
   tableSetRowSpacings table 7
   tableSetColSpacings table 7

   -- Наполнение таблицы строками
   if null model
   then genLabel (italicMeta "Пусто") >>= addCell table 0 0
   else let model' = sortBy ( \ (Parsed (Address x _ _) _)
                                (Parsed (Address y _ _) _)
                              -> x `compare` y
                            ) model
        in forM_ ([0..] `zip` model')
           $ \ (i, parsed@(Parsed (Address string _ _) _)) -> do

              -- Строка адреса
              label <- genLabel string
              labelSetSelectable label True
              miscSetAlignment label 0 0.5
              addCell table 1 i label
              --genLabel "Error not implemented" >>= addCell table 1 i

              -- Кнопка редактирования адреса
              editButton <- buttonNew
              buttonSetImage editButton
                 =<< imageNewFromStock stockEdit (IconSizeUser 1)
              after editButton buttonActivated (editAddress b parsed)
              addCell table 0 i editButton

   -- Таблица помещается в контейнер и показывается результат
   alignment <- builderGetObject b castToAlignment containerID
   destroyChildren alignment
   containerAdd alignment table
   widgetShowAll table



-- Отрисовывает дубликаты адресов.
-- Принимает набор данных и id виджета, в который вставлять результат.
drawDuplicates :: Builder -> String -> [(Parsed, Int)] -> IO ()
drawDuplicates b containerID model = do

   -- Создаю таблицу
   table <- tableNew (length model) 3 False
      -- Количество строк, столбцов, homogeneous
   tableSetRowSpacings table 7
   tableSetColSpacing table 0 7 -- номер колонки, количество пикселей

   -- Наполняю таблицу строками
   if null model
   then genLabel (italicMeta "Пусто") >>= addCell table 0 0
   else let model' = sortBy ( \ ((Parsed (Address x _ _) _), _)
                                ((Parsed (Address y _ _) _), _)
                              -> x `compare` y
                            ) model
        in addLine table `mapM_` zip [0..] model'

   -- Подставляю таблицу в контейнер и показываю результат
   alignment <- builderGetObject b castToAlignment containerID
   destroyChildren alignment
   containerAdd alignment table
   widgetShowAll table

   where

      addLine :: Table -> (Int, (Parsed, Int)) -> IO ()
      addLine table
              ( dupNumber
              ,  ( parsed@(Parsed (Address string _ _) (Right comps))
                 , dupsCount
                 )
              ) = do

         -- Ячейка с текстом адреса
         srcLabel <- genLabel string
         set srcLabel [
             widgetTooltipText := Just (format comps)
           , labelSelectable := True
           ]
         miscSetAlignment srcLabel 0 0.5
         addCell table 1 dupNumber srcLabel

         -- Кнопка редактирования адреса
         editButton <- buttonNew
         buttonSetImage editButton
            =<< imageNewFromStock stockEdit (IconSizeUser 1)
         after editButton buttonActivated (editAddress b parsed)
         addCell table 0 dupNumber editButton

         -- Ячейка с количеством дубликатов
         countLabel <- genLabel ("— " ++ show dupsCount ++ " шт.")
         addCell table 2 dupNumber countLabel
         miscSetAlignment countLabel 0 0.5



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
   else let model' = sortBy ( \ ((Parsed (Address x _ _) _), _)
                                ((Parsed (Address y _ _) _), _)
                              -> x `compare` y
                            ) model
        in addLine table `mapM_` zip [1..] model'

   -- Подставляю таблицу в контейнер и показываю результат
   alignment <- builderGetObject b castToAlignment containerID
   destroyChildren alignment
   containerAdd alignment table
   widgetShowAll table

   where


      isLeft (Left _)     = True
      isLeft (Right _)    = False

      fromLeft (Left x)   = x
      fromRight (Right x) = x


      addLine :: Table
              -> ( Int
                 ,  ( Parsed
                    , Either ErrMsg [(String, Int, Bool)]
                    )
                 )
              -> IO ()
      addLine table
              ( i
              ,  ( parsed@(Parsed (Address string _ _) (Right comps))
                 , options
                 )
              ) = do

         -- Линия-разделитель
         separator <- hSeparatorNew
         tableAttach table separator 0 3 (2*i) (2*i+1) [Fill] [] 0 0

         -- Строка адреса без пары
         leftLabel <- genLabel string
         set leftLabel [
             widgetTooltipText := Just (format comps)
           , labelSelectable := True
           ]
         --labelSetSelectable leftLabel True
         miscSetAlignment leftLabel 0 0.5

         -- Кнопка редактирования адреса
         editButton <- buttonNew
         buttonSetImage editButton
            =<< imageNewFromStock stockEdit (IconSizeUser 1)
         after editButton buttonActivated (editAddress b parsed)

         hbox <- hBoxNew False 7
         containerAdd hbox editButton
         containerAdd hbox leftLabel
         boxSetChildPacking hbox editButton PackNatural 0 PackStart
         addCell table 0 (i*2+1) hbox
         set table [ tableChildYOptions hbox := [] ]

         -- Похожие адреса
         case options of
            Left err -> do
               errorLabel <- genLabel (italicMeta err)
               labelSetSelectable errorLabel True
               miscSetAlignment errorLabel 0 0.5
               addCell table 1 (i*2+1) errorLabel
            Right options' -> do
               vbox <- vBoxNew True 7 -- homogeneous, spacing
               forM_ options' $ \ (addr, fit, matched) -> do
                  -- Создаю одну из альтернатив
                  hbox <- hBoxNew False 7
                  boxSetHomogeneous hbox False
                  alt <- genLabel addr
                  labelSetSelectable alt True
                  miscSetAlignment alt 0 0.5
                  boxPackStart hbox alt PackNatural 0
                  when matched $ do
                      pairedLabel <- genLabel (italicMeta "— уже имеет пару")
                      boxPackStart hbox pairedLabel PackNatural 0
                  boxPackEndDefaults vbox hbox -- Добавляю hbox в конец vbox
               addCell table 1 (i*2+1) vbox



editAddress :: Builder -> Parsed -> IO ()
editAddress b (Parsed (Address string origin context) parsed) = do
   -- Создаю диалоговое окно в коде, а не в glade, потому что:
   --
   -- glade не позволяет привязать к кнопкам сигнал response;
   --
   -- gtk2hs не позволяет навесить однократный обработчик сигнала при каждом 
   -- открытии диалога. Это означает, что либо при каждом открытии будут 
   -- навешиваться новые и новые обработчики на этот диалог, либо навешивать 
   -- обработчики диалога вне обработчика клика по кнпоке открытия диалога, 
   -- тогда я не буду иметь доступа к данным вроде Label отчёта;

   let parsed' = case parsed of
                    Right comps -> format comps
                    Left err    -> show err

   -- Dialog
   d <- dialogNew
   set d [ windowTitle := "Редактирование адреса" ]
   containerSetBorderWidth d 10

   -- Кнопки
   saveButton <- dialogAddButton d "Сохранить" ResponseAccept
   dialogAddButton d "Отмена"    ResponseCancel
   --dialogAddButton d "Удалить"   ResponseReject
   dialogSetDefaultResponse d ResponseAccept
   case origin of
      -- TODO: Костыль, пока не реализовано редактирование таблицы
      Photos _      -> return ()
      Notes _ _ _ _ -> do
         widgetSetSensitive saveButton False
         set saveButton [ widgetTooltipText := Just "Не реализовано" ]

   -- Table
   t <- tableNew 3 2 False -- rows columns homogeneous
   tableSetRowSpacings t 14
   tableSetColSpacings t 14
   containerSetBorderWidth t 5 -- выравниваю содержимое таблицы с кнопками
   flip containerAdd t =<< dialogGetUpper d

   -- Колонка названий
   addressLabel <- labelNew (Just "Адрес:")
   parsedLabel  <- labelNew (Just "Результат разбора:")
   sourceLabel  <- labelNew (Just "Исходник:")
   miscSetAlignment addressLabel 1 0.5 -- xAlign yAlign
   miscSetAlignment parsedLabel  1 0   -- xAlign yAlign
   miscSetAlignment sourceLabel  1 0   -- xAlign yAlign
   tableAttach t addressLabel 0 1 0 1 [Fill] [Expand, Fill] 0 0
   tableAttach t parsedLabel  0 1 1 2 [Fill] [Expand, Fill] 0 0
   tableAttach t sourceLabel  0 1 2 3 [Fill] [Expand, Fill] 0 0

   -- Колонка значений
   addressValue <- entryNew
   parsedValue  <- labelNew (Just parsed')
   sourceValue  <- labelNew (Just context)
   entrySetText addressValue string
   entrySetActivatesDefault addressValue True
   miscSetAlignment parsedValue 0 0 -- xAlign yAlign
   miscSetAlignment sourceValue 0 0 -- xAlign yAlign
   tableAttachDefaults t addressValue 1 2 0 1
   tableAttachDefaults t parsedValue  1 2 1 2
   tableAttachDefaults t sourceValue  1 2 2 3
   labelSetSelectable parsedValue True
   labelSetSelectable sourceValue True
   labelSetLineWrapMode parsedValue WrapPartialWords
   labelSetLineWrapMode sourceValue WrapPartialWords
   labelSetLineWrap parsedValue True
   labelSetLineWrap sourceValue True

   -- События
   on d response $ \ responseID -> case responseID of
      ResponseAccept -> do
         newAddr <- entryGetText addressValue
         when (newAddr /= string)
            $ case origin of
                 Notes file sheet col row -> undefined
                 Photos file -> do
                    renameFile file (replaceBaseName file newAddr)
                    compareReports b
      -- ResponseReject -> removeFile file
         -- TODO: пока удалить адрес нельзя из соображений безопасности
      _ -> return ()

   widgetShowAll d
   dialogRun d
   widgetDestroy d
