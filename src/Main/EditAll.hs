module Main.EditAll where


import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk
import Control.Monad (forM_, liftM2)
import Data.List (partition)

import Data.Types
import Main.Header (obtainPhotos)
import Utils.GTK (addCell)
import Utils.Misc (isLeft)



editAll :: Builder -> Type -> IO ()
editAll b Note  = undefined -- TODO
editAll b Photo = do
   photos <- obtainPhotos b
   case photos of
      Just photos' -> genDialog photos'
      _            -> return ()



genDialog :: [Parsed] -> IO ()
genDialog parsed = do

   -- Dialog
   d <- dialogNew
   set d [ windowTitle := "Редактирование всех адресов"
         , windowDefaultWidth  := 1000
         , windowDefaultHeight := 700
         ]
   containerSetBorderWidth d 10

   -- Кнопки
   saveButton <- dialogAddButton d "Сохранить" ResponseAccept
   dialogAddButton d "Отмена" ResponseCancel
   dialogSetDefaultResponse d ResponseAccept

   content <- genContent parsed
   flip containerAdd content =<< dialogGetUpper d

   widgetShowAll d
   dialogRun d
   widgetDestroy d



genContent :: [Parsed] -> IO ScrolledWindow
genContent parsed = do

   let (lefts, rights) = partition (isLeft . parsedComps) parsed

   vbox <- vBoxNew False 15 -- homogeneous spacing

   -- Not parsed
   containerAdd vbox =<< genFrame "Не поддаются анализу" lefts
      ( \ table i (Parsed (Address string _ _) _) -> do
           addEntry table 0 i string
           addEntry table 1 i string
      )

   -- Parsed
   containerAdd vbox =<< genFrame "Разобранные" rights
      ( \ table i (Parsed (Address string _ _) _) -> do
           addEntry table 0 i string
           addEntry table 1 i string -- FIXME: нормализуй адрес
      )

   scrolled <- scrolledWindowNew Nothing Nothing
   scrolledWindowAddWithViewport scrolled vbox
   return scrolled

   where
      addEntry :: Table -> Int -> Int -> String -> IO ()
      addEntry table x y text = do
         entry <- entryNew
         entrySetText entry text
         addCell table x y entry



genFrame :: String -> [Parsed]
         -> (Table -> Int -> Parsed -> IO ())
         -> IO Frame
genFrame header parsed genLine = do
   frame <- frameNew
   frameSetLabel frame header
   table <- tableNew 1 2 False -- rows columns homogeneous
   containerAdd frame table
   forM_ ([0..] `zip` parsed) $ \ (i, p) -> genLine table i p
   return frame
