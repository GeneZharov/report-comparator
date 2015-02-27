REM Скрипт для запуска с помощью runghc

SET sdir=%~dp0
pushd %sdir\..\src
runghc Main\Main.hs
popd
