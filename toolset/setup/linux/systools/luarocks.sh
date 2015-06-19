#!/bin/bash

VERSION="2.2.1"
LUAROCKS=$IROOT/luarocks-$VERSION
RETCODE=$(fw_exists ${LUAROCKS}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $LUAROCKS.installed
  return 0; }

fw_depends lua

fw_get -O http://luarocks.org/releases/luarocks-2.2.1.tar.gz
fw_untar luarocks-2.2.1.tar.gz

cd $LUAROCKS
./configure --prefix=$LUAROCKS --with-lua=$LUA_HOME
make bootstrap

echo "export LUAROCKS_HOME=${LUAROCKS}" > $LUAROCKS.installed
echo -e "export PATH=${LUAROCKS}/bin:\$PATH" >> $LUAROCKS.installed

source $LUAROCKS.installed
