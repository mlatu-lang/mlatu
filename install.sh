#!/bin/bash

function if_doesnt_exist {
  if [ "$(which "$1")" = "" ] 
  then 
  $2
  fi
}

if_doesnt_exist esy 'npm install --global esy --quiet'

esy install --quiet 
esy build --quiet 
cp -f "$(esy echo "#{self.target_dir / 'default' / 'bin' / 'main.exe'}")" /usr/local/bin/mlatu

if_doesnt_exist mlatu "echo \"'/usr/local/bin' is not in your \$PATH variable. Add it to be able to use mlatu.\""