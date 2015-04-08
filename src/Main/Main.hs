import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Data.Maybe (isNothing, fromJust)
import Data.ByteString.Char8 (pack)
import Codec.Binary.UTF8.Light (decode)
import Data.List (sortBy, isInfixOf)
import Text.Parsec.Error (ParseError)
import Control.Exception
import System.IO.Error (ioeGetFileName)
import System.Process (readProcess)
import Debug.Trace

import Main.Extraction (extract, fromNotes, fromPhotos)
import Main.Analysis (matchedCount, duplicates, notParsed, notMatched)
import Main.Utils (alert, format)
import Address.Types



-- Обработчик выбора файла с табличным отчётом, обновляет меню страниц таблицы
updateSheets :: Builder -> IO ()
updateSheets b = do
    file <- builderGetObject b castToFileChooserButton "notes"
            >>= liftM fromJust . fileChooserGetFilename
    try (readProcess "./spreadsheets/sheet-names" [file] "")
        >>= update file
    where update :: String -> Either IOError String -> IO ()

          update file (Left parseErr) = do

              -- Очищаю меню табличного отчёта
              mainWindow <- builderGetObject b castToWindow "mainWindow"
              notes <- builderGetObject b castToFileChooserButton "notes"
              fileChooserUnselectAll notes

              -- Очищаю меню страниц табличного отчёта
              notesSheets <- builderGetObject b castToComboBox "notesSheets"
              comboBoxSetModelText notesSheets

              alert mainWindow (file ++ " не является электронной таблицей")

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

    photosDir <- fileChooserGetFilename photos
    notesFile <- fileChooserGetFilename notes
    dirMode   <- liftM (== 0) (comboBoxGetActive photosDirMode)
    colNum    <- spinButtonGetValueAsInt notesColumn
    sheetName <- liftM fromJust (comboBoxGetActiveText notesSheets)

    --let photosDir = Just "../samples/epil"
    --let notesFile = Just "../samples/epil.csv"
    --let photosDir = Just "../samples/spb"
    --let notesFile = Just "../samples/spb.csv"
    --let photosDir = Just "/media/b1/moscow"
    --let notesFile = Just "/media/b1/moscow.csv"
    --let photosDir = Just "/media/b1/mo"
    --let notesFile = Just "/media/b1/mo.csv"

    if any isNothing [photosDir, notesFile]
    then do
        mainWindow <- builderGetObject b castToWindow "mainWindow"
        alert mainWindow "Заданы не все данные"
    else let getFileName = decode . pack . fromJust
         in do photos <- try $ extract (fromPhotos dirMode)
                                       (getFileName photosDir)
               notes  <- try $ extract (fromNotes sheetName (colNum - 1))
                                       (getFileName notesFile)
               draw b photos notes
                   -- `catch` \ (e :: SomeException)
                   --        -> alert mainWindow (show e)



-- Отрисовывает результат сопоставления отчётов
draw :: Builder
     -> Either IOError [ (String, Either ParseError [Component]) ]
     -> Either IOError [ (String, Either ParseError [Component]) ]
     -> IO ()

draw b (Left photosErr) _ = do
    mainWindow <- builderGetObject b castToWindow "mainWindow"
    alert mainWindow (show photosErr)
draw b _ (Left notesErr) = do
    mainWindow <- builderGetObject b castToWindow "mainWindow"
    alert mainWindow $
        if encErr `isInfixOf` show notesErr
        then "Неверная кодировка файла: " ++ fromJust (ioeGetFileName notesErr)
          ++ "\nОжидается кодировка UTF-8"
        else "Ошибка разбора файла:\n" ++ show notesErr
        where encErr = "hGetContents: invalid argument (invalid byte sequence)"

draw b (Right photos) (Right notes) = do

    -- Количество адресов с парой
    let matched = matchedCount photos notes
    drawMatched b "photosMatched" matched
    drawMatched b "notesMatched" matched
    --print matched

    -- Адреса, которые не удалось распарсить
    drawNotParsed b "photosNotParsed" $ notParsed photos
    drawNotParsed b "notesNotParsed"  $ notParsed notes
    --print $ notParsed photos
    --print $ notParsed notes

    -- Дубликаты
    drawDuplicates b "photosDuplicates" $ duplicates photos
    drawDuplicates b "notesDuplicates"  $ duplicates notes

    -- Адреса без пары
    drawNotMatched b "photosNotMatched" $ notMatched photos notes
    drawNotMatched b "notesNotMatched"  $ notMatched notes photos
    --print $ notMatched notes photos
    --print $ notMatched photos notes

    when (matched == 0) $ do
        mainWindow <- builderGetObject b castToWindow "mainWindow"
        alert mainWindow
            "Нет ни одного совпадения адресов.\n\n\
            \Возможно отчёты полностью отличаются \
            \или из них неверно извлечены адреса:\n\n\
            \• Возможно в таблице адреса лежат не в 3-й колонке;\n\n\
            \• Возможно внутри каталога фотографий находится лишний уровень \
            \вложенности вместо просто набора файлов/каталогов с адресами в \
            \именах;"



drawMatched :: Builder -> String -> Int -> IO ()
drawMatched b labelID count = do
    label <- builderGetObject b castToLabel labelID
    labelSetMarkup label (show count)



-- Отрисовывает дубликаты адресов.
-- Принимает набор данных и id виджета, в который вставлять результат.
drawDuplicates :: Builder -> String
    -> [ (Int, (String, Either ParseError [Component])) ]
    -> IO ()
drawDuplicates b containerID model = do

    -- Создаю таблицу
    table <- tableNew (length model) 2 False
        -- Количество строк, столбцов, homogeneous
    tableSetRowSpacings table 7
    tableSetColSpacing table 0 7 -- номер колонки, количество пикселей

    -- Наполняю таблицу строками
    forM_ (zip [0..] model) $ \ (i, (count, (src, Right comps))) -> do

        -- Ячейка с текстом адреса
        srcLabel <- genLabel src
        set srcLabel [
            widgetTooltipText := Just (format comps)
          , labelSelectable := True
          ]
        addCell table 0 i srcLabel

        -- Ячейка с количеством дубликатов
        genLabel ("— " ++ show count ++ " шт.") >>= addCell table 1 i

    -- Подставляю таблицу в контейнер и показываю результат
    alignment <- builderGetObject b castToAlignment containerID
    containerGetChildren alignment >>= mapM_ widgetDestroy
    containerAdd alignment table
    widgetShowAll table



-- Отрисовывает не распарсенные адреса.
-- Принимает набор данных и id виджета, в который вставлять результат.
drawNotParsed :: Builder -> String
              -> [ (String, Either ParseError [Component]) ]
              -> IO ()
drawNotParsed b containerID model = do

    -- Создаю таблицу
    table <- tableNew (length model) 2 False
        -- Количество строк, столбцов, homogeneous
    tableSetRowSpacings table 7

    -- Наполняю таблицу строками
    forM_ (zip [0..] model) $ \ (i, addr) -> do
        label <- genLabel (fst addr)
        labelSetSelectable label True
        addCell table 0 i label
        --genLabel "Error not implemented" >>= addCell table 1 i

    -- Подставляю таблицу в контейнер и показываю результат
    alignment <- builderGetObject b castToAlignment containerID
    containerGetChildren alignment >>= mapM_ widgetDestroy
    containerAdd alignment table
    widgetShowAll table



-- Отрисовывает адреса без пары. Принимает набор данных и идентификатор 
-- виджета, в который вставлять результат.
drawNotMatched :: Builder -> String
               -> [(
                      String,
                      [Component],
                      Either String [(String, Int, Bool)]
                  )]
               -> IO ()
drawNotMatched b containerID model = do

    -- Создаю таблицу для адресов без пар
    table <- tableNew (length model) 2 False -- строк, столбцов, homogeneous
    tableSetRowSpacings table 7
    tableSetColSpacings table 40

    genLabel (meta "Без пары из текущей группы адресов") >>= addCell table 0 0
    genLabel (meta "Похожие из другой группы адресов" )  >>= addCell table 1 0

    -- Генерю строки с адресами, которым не нашлось пары
    forM_ (zip model [1..]) $ \ ((addr, comps, options), i) -> do

        -- Линия-разделитель
        separator <- hSeparatorNew
        tableAttach table separator 0 2 (2*i) (2*i+1) [Fill] [] 0 0

        -- Строка адреса без пары
        leftLabel <- genLabel addr
        set leftLabel [
            widgetTooltipText := Just (format comps)
          , labelSelectable := True
          ]
        --labelSetSelectable leftLabel True
        addCell table 0 (i*2+1) leftLabel

        -- Похожие адреса
        if isLeft options
        then do
            errorLabel <- genLabel (italicMeta $ fromLeft options)
            labelSetSelectable errorLabel True
            addCell table 1 (i*2+1) errorLabel
        else do
            vbox <- vBoxNew True 7 -- homogeneous, spacing
            let sorted = flip sortBy (fromRight options)
                         $ \ (_, a, _) (_, b, _) -> compare a b
                         -- Сортирую по количеству ошибок
            forM_ sorted $ \ (addr, fit, matched) -> do
                -- Генерю одну из альтернатив
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
    containerGetChildren alignment >>= mapM_ widgetDestroy
    containerAdd alignment table
    widgetShowAll table

    where isLeft (Left _)  = True
          isLeft (Right _) = False
          fromLeft (Left x)   = x
          fromRight (Right x) = x
          meta text = "<span fgcolor=\"#6D6D6D\" \
                      \>" ++ text ++ "</span>"
          italicMeta text = "<span fgcolor=\"#6D6D6D\" \
                                 \ style=\"italic\" \
                            \>" ++ text ++ "</span>"



genLabel text = do
    label <- labelNew Nothing
    miscSetAlignment label  0 0
    labelSetMarkup label text
    return label

addCell table x y widget = tableAttach table widget
    x (x+1)       -- Колонка слева/справа
    y (y+1)       -- Строка сверху/снизу
    [Fill] [Fill] -- Horizontal/vertical resizing
    0 0           -- padding горизонтальный/вертикальный



main :: IO ()
main = do

    initGUI
    b <- builderNew
    builderAddFromFile b "./Main/main.glade"
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
