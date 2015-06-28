module Main.Body where



import Data.List (sortBy)
import Data.Ord (comparing)
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk
import Control.Monad

import Utils.Addr
import Utils.GTK
import Data.Types
import Data.Analysis (matchedCount, duplicates, notParsed, notMatched)



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
