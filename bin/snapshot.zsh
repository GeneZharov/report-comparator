#!/bin/zsh

# find -type f > ~/snapshot

snapshot=$1
files=("${(@f)$(cat $snapshot)}")

for file ($files) {
  echo Creating $file
  dir=$(dirname $file)
  if [[ ! -d $dir ]] { mkdir -p $dir }
  touch "$file"
}
