#!/bin/bash

fw_installed nim && return 0

NIM_VERSION="0.11.2"
NIM_CSOURCES="6bf2282"

fw_get -O https://github.com/nim-lang/Nim/archive/v$NIM_VERSION.tar.gz
fw_untar v$NIM_VERSION.tar.gz
mv Nim-$NIM_VERSION nim
cd nim

git clone git://github.com/nim-lang/csources.git
cd csources
git checkout $NIM_CSOURCES
sh build.sh
cd ..

bin/nim c koch

# bootstrapping nim's compiler
./koch boot -d:release

echo "export NIM_HOME=${IROOT}/nim" > $IROOT/nim.installed
echo -e "export PATH=\$NIM_HOME/bin:\$PATH" >> $IROOT/nim.installed

source $IROOT/nim.installed

