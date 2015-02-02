import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Data.Maybe (isNothing, fromJust)
import Data.ByteString.Char8 (pack)
import Codec.Binary.UTF8.Light (decode)

import Main.Extraction (extract, fromNotes, fromPhotos)
import Main.Analysis (notParsed, notMatched)



process builder notesFile photosDir = do

    notes  <- extract fromNotes  "/root/s/zdrav/csv/from-excel.csv"
    photos <- extract fromPhotos "/root/s/zdrav/отчёты/Фото Эпилепсия 09.2014"

    --notes  <- extract fromNotes  notesFile
    --photos <- extract fromPhotos photosDir

    -- Адреса, которые не удалось распарсить
    --print $ notParsed notes
    --print $ notParsed photos

    -- Адреса отчёта без пары
    --print $ notMatched notes photos
    --print $ notMatched photos notes

    -- Удаляю заглушки
    photosNotParsed  <- builderGetObject builder castToAlignment "photosNotParsed"
    --photosVP  <- builderGetObject builder castToViewport "photosVP"
    --notesVP   <- builderGetObject builder castToViewport "notesVP"
    --photosCap <- builderGetObject builder castToLabel "photosCap"
    --notesCap  <- builderGetObject builder castToLabel "notesCap"
    --containerRemove photosVP photosCap
    --containerRemove notesVP notesCap

    let model = notParsed notes
    photosTable <- tableNew (length model) -- Количество строк
                            2              -- Количество столбцов
                            True           -- Homogeneous
    tableSetRowSpacings photosTable 7
    forM_ (zip model [0..]) $ \ (addr, i) -> do
        addrLabel  <- labelNew Nothing
        errorLabel <- labelNew Nothing
        labelSetMarkup addrLabel ("<big>" ++ addr ++ "</big>")
        labelSetMarkup errorLabel "<big>Error not implemented</big>"
        labelSetSelectable addrLabel True
        labelSetSelectable errorLabel True
        miscSetAlignment addrLabel 0 0.5
        miscSetAlignment errorLabel 0 0.5
        tableAttach photosTable
                    addrLabel
                    0 -- Колонка слева
                    1 -- Колонка справа
                    i -- Строка сверху
                    (i + 1) -- Колонка снизу
                    [Fill] -- Horizontal resizing
                    [] -- Vertical resizing
                    20 0 -- padding горизонтальный и вертикальный
        tableAttach photosTable
                    errorLabel
                    1 -- Колонка слева
                    2 -- Колонка справа
                    i -- Строка сверху
                    (i + 1) -- Колонка снизу
                    [Fill] -- Horizontal resizing
                    [] -- Vertical resizing
                    20 0 -- padding горизонтальный и вертикальный
    containerAdd photosNotParsed photosTable
    widgetShowAll photosNotParsed



main :: IO ()
main = do

    initGUI
    builder <- builderNew
    builderAddFromFile builder "./Main/main.glade"
    mainWindow <- builderGetObject builder castToWindow "window1"
    onDestroy mainWindow mainQuit

    photosW <- builderGetObject builder castToFileChooserButton "photos"
    notesW  <- builderGetObject builder castToFileChooserButton "notes"
    submitW <- builderGetObject builder castToButton "submit"
    alertW  <- builderGetObject builder castToMessageDialog "alert"

    onResponse alertW $ const (widgetHide alertW) -- Сокрытие попапа по "Ok"
    onClicked submitW $ do
        process builder "" ""
        {-
        notesFile <- fileChooserGetFilename notesW
        photosDir <- fileChooserGetFilename photosW

        if any isNothing [notesFile, photosDir]
        if False
        then widgetShow alertW
        else let getFileName = decode . pack . fromJust
             in  process builder (getFileName notesFile) (getFileName photosDir)
        -}

    widgetShowAll mainWindow
    mainGUI
