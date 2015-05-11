REM можно заменить этот пакетный файл на ярлык

SET sdir=%~dp0
%sdir:~0,-1%\..\.cabal-sandbox\bin\comparator.exe
