#!/bin/bash

# Скрипт для разворачивания программы под windows
#
# Запускать надо через git bash (mingw32) или cygwin,
# так как process-1.2.3.0 требует для сборки configure

cd $(dirname $BASH_SOURCE)/..

git pull
pushd parser & git pull & popd

cabal update
cabal install

read -n 1 -p "Press any key..."
