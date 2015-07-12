#!/bin/bash

# Скрипт для разворачивания программы под windows
#
# Запускать надо через git bash (mingw32) или cygwin,
# так как process-1.2.3.0 требует для сборки configure

cd $(dirname $BASH_SOURCE)/..

git clone 'https://github.com/kahless/russian-address-parser' parser

cabal update
cabal install gtk2hs-buildtools & REM потребуется для сборки gtk2hs
cabal sandbox init
cabal sandbox add-source parser
cabal install

read -n 1 -p "Press any key..."
