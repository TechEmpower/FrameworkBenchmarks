#!/bin/bash

NIM_VERSION="v0.10.2"
NIM=$IROOT/nim
RETCODE=$(fw_exists ${NIM}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $NIM.installed
  return 0; }

git clone git://github.com/Araq/Nim.git nim
cd nim
# post version 0.10.2 - most recent as of 2014-12-
git checkout $NIM_VERSION

# Fixes a complex http request issue in 0.10.2:
# https://github.com/Araq/Nim/pull/1848
fw_get https://patch-diff.githubusercontent.com/raw/Araq/Nim/pull/1848.patch
git apply 1848.patch
rm 1848.patch

git clone git://github.com/nim-lang/csources.git
cd csources
sh build.sh
cd ..

bin/nim c koch

# bootstrapping nim's compiler
./koch boot -d:release

echo "export NIM_HOME=${NIM}" > $NIM.installed
echo -e "export PATH=${NIM}/bin:\$PATH" >> $NIM.installed

source $NIM.installed
