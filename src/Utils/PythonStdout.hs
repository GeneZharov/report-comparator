module Utils.PythonStdout where


import System.Environment (getEnvironment)
import System.IO
import System.Process


-- Утилита для получения стандартного вывода из дочернего процесса. Создавалась 
-- специально для питона, чтобы принудить его писать в utf-8, так как в windows 
-- питон сбоит из-за того, что не умеет писать в дефолтной системной кодировке.
pythonStdout :: CreateProcess -> IO String
pythonStdout conf = do
   env <- getEnvironment
   (_, Just outH, _, _) <- createProcess conf
      {
         std_out = CreatePipe
      ,  env     = Just $ ("PYTHONIOENCODING", "utf_8") : env
      }
   hSetEncoding outH utf8
   hGetContents outH
