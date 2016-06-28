#!/bin/bash

fw_depends lua

RETCODE=$(fw_exists ${IROOT}/luarocks.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/luarocks.installed
  return 0; }

LUAROCKS_VERSION="2.2.1"
LUAROCKS=$IROOT/luarocks-$LUAROCKS_VERSION

fw_get -O http://luarocks.org/releases/luarocks-$LUAROCKS_VERSION.tar.gz
fw_untar luarocks-$LUAROCKS_VERSION.tar.gz

cd $LUAROCKS
./configure --prefix=$LUA_HOME --with-lua=$LUA_HOME
make --quiet bootstrap

echo "" > $IROOT/luarocks.installed

source $IROOT/luarocks.installed
