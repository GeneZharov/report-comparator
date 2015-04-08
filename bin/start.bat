REM Скрипт для запуска с помощью runghc

SET sdir=%~dp0
pushd %sdir:~0,-1%\..\src
runghc Main\Main.hs
popd
