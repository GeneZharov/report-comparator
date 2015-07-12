-- Утилита для получения стандартного вывода из дочернего процесса. Создавалась 
-- специально для питона, чтобы принудить его писать в utf-8, так как в windows 
-- питон сбоит из-за того, что не умеет писать в дефолтной системной кодировке.


module Utils.PythonStdout where


import System.Environment (getEnvironment)
import System.IO
import System.Process
import System.Exit
import Control.Exception (throwIO)


-- Сочетание waitForProcess и hGetContents подвешивают процесс python'а для 
-- некотоых xls-файлов. Если убрать waitForProcess или hGetContents, то всё 
-- работает. Если вместо содержимого ячеек распечатывать любой другой русский 
-- текст, то всё работает. Но вместе — не работает. Это необъяснимо. Пробовал 
-- использовать разные версии process помимо 1.2.3.0. Заменить 
-- waitForProcess+hGetContents на readCreateProcessWithExitCode не могу, так 
-- как последний читает вывод питона в системной виндовой кодировке (cp1252), а 
-- русский текст в ней не кодируется. Изменить системную кодировку локально для 
-- этого вызова или всей хаскельной программы нельзя — только целиком для 
-- системы.
{-
pythonStdout :: CreateProcess -> IO String
pythonStdout conf = do

   env <- getEnvironment
   (_, Just outH, _, p) <-
      createProcess conf
         { std_out = CreatePipe
         --, env = Just $ ("PYTHONIOENCODING", "utf_8") : env
         , env = Just $ ("PYTHONIOENCODING", "UTF-8") : env
         }

   code <- waitForProcess p
   case code of
      ExitSuccess -> do
         hSetEncoding outH utf8
         hGetContents outH
      ExitFailure code' ->
         let text = unlines [ ("process exit code: " ++ show code')
                            ,  "stdout: " ++ stdout
                            ,  "stderr: " ++ stderr
                            ]
         in throwIO (userError text)
-}


-- Рабочая версия, но не сообщает об ошибке в коде возврата
pythonStdout :: CreateProcess -> IO String
pythonStdout conf = do
    env <- getEnvironment
    (_, Just outH, _, _) <- createProcess conf
        { std_out = CreatePipe
        , env = Just (("PYTHONIOENCODING", "utf_8") : env)
        }
    hSetEncoding outH utf8
    hGetContents outH
