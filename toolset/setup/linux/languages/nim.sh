#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/nim.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/nim.installed
  return 0; }
  
NIM_VERSION="0.11.2"

fw_get -O https://github.com/nim-lang/Nim/archive/v$NIM_VERSION.tar.gz
fw_untar v$NIM_VERSION.tar.gz
mv Nim-$NIM_VERSION nim
cd nim

git clone git://github.com/nim-lang/csources.git
cd csources
sh build.sh
cd ..

bin/nim c koch

# bootstrapping nim's compiler
./koch boot -d:release

echo "export NIM_HOME=${IROOT}/nim" > $IROOT/nim.installed
echo -e "export PATH=\$NIM_HOME/bin:\$PATH" >> $IROOT/nim.installed

source $IROOT/nim.installed
