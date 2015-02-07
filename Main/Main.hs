import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Data.Maybe (isNothing, fromJust)
import Data.ByteString.Char8 (pack)
import Codec.Binary.UTF8.Light (decode)
import Data.List (sortBy)
import Debug.Trace (trace)

import Main.Extraction (extract, fromNotes, fromPhotos)
import Main.Analysis (notParsed, notMatched)



-- Отрисовывает результат вычислений
draw builder photosDir notesFile = do

    -- Извлекаю адреса из файлов
    --photos <- extract fromNotes notesFile
    --notes  <- extract fromNotes notesFile
    photos <- extract fromPhotos "./samples/epil"
    notes  <- extract fromNotes  "./samples/from-excel.csv"

    -- Адреса, которые не удалось распарсить
    drawNotParsed builder "photosNotParsed" $ notParsed photos
    drawNotParsed builder "notesNotParsed"  $ notParsed notes
    --print $ notParsed photos
    --print $ notParsed notes

    -- Адреса без пары
    drawNotMatched builder "photosNotMatched" $ notMatched photos notes
    drawNotMatched builder "notesNotMatched"  $ notMatched notes photos
    --print $ notMatched notes photos
    --print $ notMatched photos notes



-- Отрисовывает не распарсенные адреса. Принимает набор данных и идентификатор 
-- виджета, в который вставлять результат.
drawNotParsed builder containerID model = do

    -- Создаю таблицу
    table <- tableNew (length model) 2 False
        -- Количество строк, столбцов, homogeneous
    tableSetRowSpacings table 7

    -- Наполняю таблицу строками
    forM_ (zip model [0..]) $ \ (addr, i) -> do
        label <- genLabel addr
        labelSetSelectable label True
        addCell table 0 i label
        --genLabel "Error not implemented" >>= addCell table 1 i

    -- Подставляю таблицу в контейнер и показываю результат
    alignment <- builderGetObject builder castToAlignment containerID
    containerGetChildren alignment >>= mapM_ widgetDestroy
    containerAdd alignment table
    widgetShowAll table



-- Отрисовывает адреса без пары. Принимает набор данных и идентификатор 
-- виджета, в который вставлять результат.
drawNotMatched builder containerID model = do

    -- Создаю таблицу для адресов без пар
    table <- tableNew (length model) 2 False -- строк, столбцов, homogeneous
    tableSetRowSpacings table 7

    genLabel (header "Без пары из текущей группы адресов") >>= addCell table 0 0
    genLabel (header "Похожие из другой группы адресов" )  >>= addCell table 1 0

    -- Генерю строки с адресами, которым не нашлось пары
    forM_ (zip model [1..]) $ \ ((addr, options), i) -> do

        -- Линия-разделитель
        separator <- hSeparatorNew
        tableAttach table separator 0 2 (2*i) (2*i+1) [Fill] [] 0 0

        -- Строка адреса без пары
        leftLabel <- genLabel addr
        labelSetSelectable leftLabel True
        addCell table 0 (i*2+1) leftLabel

        -- Похожие адреса
        if isLeft options
        then do
            errorLabel <- genLabel (fromLeft options)
            labelSetSelectable errorLabel True
            addCell table 1 (i*2) errorLabel
        else do
            vbox <- vBoxNew True 7 -- homogeneous, spacing
            let sorted = flip sortBy (fromRight options)
                         $ \ (_, a, _) (_, b, _) -> compare a b
                         -- Сортирую по количеству ошибок
            forM_ sorted $ \ (addr, fit, matched) -> do
                -- Генерю одну из альтернатив
                hbox <- hBoxNew False 7
                boxSetHomogeneous hbox False
                if matched
                    then genLabel "— уже имеет пару"
                     >>= boxPackEndDefaults hbox
                    else return ()
                alt <- genLabel addr
                labelSetSelectable alt True
                boxPackEndDefaults hbox alt
                boxPackEndDefaults vbox hbox -- Добавляю hbox в конец vbox
            addCell table 1 (i*2+1) vbox

    -- Подставляю таблицу в контейнер и показываю результат
    alignment <- builderGetObject builder castToAlignment containerID
    containerGetChildren alignment >>= mapM_ widgetDestroy
    containerAdd alignment table
    widgetShowAll table

    where isLeft (Left _)  = True
          isLeft (Right _) = False
          fromLeft (Left x)   = x
          fromRight (Right x) = x
          header text = "<span fgcolor=\"#6D6D6D\" \
                             \ style=\"italic\" \
                        \>" ++ text ++ "</span>"



genLabel text = do
    label <- labelNew Nothing
    miscSetAlignment label  0 0
    labelSetMarkup label $ "<big>" ++ text ++ "</big>"
    return label

addCell table x y widget = tableAttach table widget
    x (x+1)       -- Колонка слева/справа
    y (y+1)       -- Строка сверху/снизу
    [Fill] [Fill] -- Horizontal/vertical resizing
    20 0          -- padding горизонтальный/вертикальный



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
