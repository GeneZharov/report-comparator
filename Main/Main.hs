import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Data.Maybe (isNothing, fromJust)
import Data.ByteString.Char8 (pack)
import Codec.Binary.UTF8.Light (decode)

import Main.Extraction (extract, fromNotes, fromPhotos)
import Main.Analysis (notParsed, notMatched)



-- Отрисовывает результат вычислений
draw builder photosDir notesFile = do

    -- Извлекаю адреса из файлов
    --notes  <- extract fromNotes notesFile
    --photos <- extract fromNotes notesFile
    notes  <- extract fromNotes  "/root/s/zdrav/csv/from-excel.csv"
    photos <- extract fromPhotos "/root/s/zdrav/отчёты/Фото Эпилепсия 09.2014"

    -- Адреса, которые не удалось распарсить
    drawNotParsed builder "photosNotParsed" $ notParsed notes
    drawNotParsed builder "notesNotParsed"  $ notParsed photos
    --print $ notParsed notes
    --print $ notParsed photos

    -- Адреса без пары
    drawNotMatched builder "notesNotMatched"  $ notMatched notes photos
    drawNotMatched builder "photosNotMatched" $ notMatched photos notes
    --print $ notMatched notes photos
    --print $ notMatched photos notes




-- Отрисовывает не распарсенные адреса. Принимает набор данных и идентификатор 
-- виджета, в который вставлять результат.
drawNotParsed builder containerID model = do

    -- Создаю таблицу
    table <- tableNew (length model) 2 True
        -- Количество строк, столбцов, homogeneous
    tableSetRowSpacings table 7

    -- Наполняю таблицу строками
    forM_ (zip model [0..]) $ \ (addr, i) -> do
        addrLabel  <- labelNew Nothing
        errorLabel <- labelNew Nothing
        labelSetMarkup addrLabel $ "<big>" ++ addr ++ "</big>"
        labelSetMarkup errorLabel "<big>Error not implemented</big>"
        labelSetSelectable addrLabel  True
        labelSetSelectable errorLabel True
        miscSetAlignment addrLabel  0 0.5
        miscSetAlignment errorLabel 0 0.5
        tableAttach table addrLabel
                    0 1       -- Колонка слева/справа
                    i (i+1)   -- Строка сверху/снизу
                    [Fill] [] -- Horizontal/vertical resizing
                    20 0      -- padding горизонтальный/вертикальный
        tableAttach table errorLabel
                    1 2       -- Колонка слева/справа
                    i (i+1)   -- Строка сверху / снизу
                    [Fill] [] -- Horizontal/vertical resizing
                    20 0      -- padding горизонтальный и вертикальный

    -- Подставляю таблицу в контейнер и показываю результат
    alignment <- builderGetObject builder castToAlignment containerID
    containerAdd alignment table
    widgetShowAll table



-- Отрисовывает адреса без пары. Принимает набор данных и идентификатор 
-- виджета, в который вставлять результат.
drawNotMatched builder containerID model = do

    -- Создаю таблицу для адресов без пар
    table <- tableNew (length model) 2 True
        -- Количество строк, столбцов, homogeneous
    tableSetRowSpacings table 7

    -- Генерю строки с адресами, которым не нашлось пары
    forM_ (zip model [0..]) $ \ ((addr, options), i) -> do

        leftLabel <- genLabel addr
        tableAttach table leftLabel
                    0 1       -- Колонка слева/справа
                    i (i+1)   -- Строка сверху/снизу
                    [Fill] [] -- Horizontal/vertical resizing
                    20 0      -- padding горизонтальный/вертикальный

        if isLeft options
        then do
            errorLabel <- genLabel (fromLeft options)
            tableAttach table errorLabel
                        1 2       -- Колонка слева/справа
                        i (i+1)   -- Строка сверху / снизу
                        [Fill] [] -- Horizontal/vertical resizing
                        20 0      -- padding горизонтальный и вертикальный
        else do
            vbox <- vBoxNew True 7 -- homogeneous, spacing
            forM_ (fromRight options) $ \ (addr, fit, matched) -> do
                -- Генерю одну из альтернатив
                hbox <- hBoxNew True 7
                boxSetHomogeneous hbox False
                genLabel addr >>= boxPackEndDefaults hbox
                genLabel (show fit) >>= boxPackEndDefaults hbox
                genLabel (show matched) >>= boxPackEndDefaults hbox
                boxPackEndDefaults vbox hbox -- Добавляю hbox в конец vbox
            tableAttach table vbox
                        1 2       -- Колонка слева/справа
                        i (i+1)   -- Строка сверху / снизу
                        [Fill] [] -- Horizontal/vertical resizing
                        20 0      -- padding горизонтальный и вертикальный

    -- Подставляю таблицу в контейнер и показываю результат
    alignment <- builderGetObject builder castToAlignment containerID
    containerAdd alignment table
    widgetShowAll table

    where isLeft (Left _)  = True
          isLeft (Right _) = False
          fromLeft (Left x)   = x
          fromRight (Right x) = x
          genLabel text = do
              label <- labelNew Nothing
              miscSetAlignment label  0 0.5
              labelSetSelectable label True
              labelSetMarkup label $ "<big>" ++ text ++ "</big>"
              return label



main = do

    initGUI
    builder <- builderNew
    builderAddFromFile builder "./Main/main.glade"
    mainWindow <- builderGetObject builder castToWindow "window1"
    onDestroy mainWindow mainQuit

    -- Alert с ошибкой
    -- TODO: А не сделать ли функцию генерации таких алертов из любого текста?
    alert <- builderGetObject builder castToMessageDialog "alert"
    onResponse alert $ const (widgetHide alert) -- Сокрытие попапа по "Ok"

    photos <- builderGetObject builder castToFileChooserButton "photos"
    notes  <- builderGetObject builder castToFileChooserButton "notes"
    submit <- builderGetObject builder castToButton "submit"

    onClicked submit $ do
        draw builder "" ""
        {-
        photosDir <- fileChooserGetFilename photos
        notesFile <- fileChooserGetFilename notes

        if any isNothing [photosDir, notesFile]
        if False
        then widgetShow alert
        else let getFileName = decode . pack . fromJust
             in  process builder (getFileName photosDir) (getFileName notesFile)
        -}

    widgetShowAll mainWindow
    mainGUI
