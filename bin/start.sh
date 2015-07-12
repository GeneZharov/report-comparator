#!/bin/bash
sdir=$(dirname $BASH_SOURCE)/..
$sdir/.cabal-sandbox/bin/a-comp.exe
if (( $? ))
  then read -n 1 -p "Press any key..."
fi
