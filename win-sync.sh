#!/bin/bash

# Скрипт для тестирования новой версии в Windows. Копирует исходники из 
# юниксового каталога, где ведётся основная разработка в тестовый каталог 
# проекта под виндой, чтобы винда могла собрать с актуальными исходниками.
#
# Не кладу скрипт в bin, так как он не сможет сам себя перетереть.

unix=/e/p/zd/a-comp         # Юниксовый каталог проекта
win=$(dirname $BASH_SOURCE) # Виндовый каталог проекта

# Копируемые файлы
files=( bin
        data
        src
        Paths_report-comparator.hs
        report-comparator.cabal
      )

rm -r "${files[@]/#/$win/}"
cp -r "${files[@]/#/$unix/}" $win
#rsync -rt --delete "${files[@]/#/$unix/}" $win # rsync нет в гите

read -n 1 -p "Press any key..."
