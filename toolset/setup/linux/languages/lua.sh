#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/lua.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/lua.installed
  return 0; }

LUA_VERSION="5.1"
LUA_MICRO="5"

fw_get -O https://github.com/LuaDist/lua/archive/$LUA_VERSION.$LUA_MICRO.tar.gz
fw_untar $LUA_VERSION.$LUA_MICRO.tar.gz

LUA_HOME=$IROOT/lua-$LUA_VERSION.$LUA_MICRO

cd $LUA_HOME
cp src/luaconf.h.orig src/luaconf.h
make linux
cd src
mkdir ../bin ../include ../lib
install -p -m 0755 lua luac ../bin
install -p -m 0644 lua.h luaconf.h lualib.h lauxlib.h ../include
install -p -m 0644 liblua.a ../lib

echo "export LUA_HOME=${LUA_HOME}" > $IROOT/lua.installed
echo "export LUA_VERSION=${LUA_VERSION}" >> $IROOT/lua.installed
echo "export LUA_MICRO=${LUA_MICRO}" >> $IROOT/lua.installed
echo -e "export LUA=${IROOT}/lua\$LUA_VERSION.\$LUA_MICRO" >> $IROOT/lua.installed
echo -e "export PATH=\$LUA_HOME/bin:\$PATH" >> $IROOT/lua.installed
# TODO: This feels very hackish
echo -e 'export LUA_PATH="./?.lua;./?.lc;$LUA_HOME/share/lua/5.1/?/init.lua;$LUA_HOME/share/lua/5.1/?.lua;$LUA_HOME/lib/lua/5.1/?/init.lua;$LUA_HOME/lib/lua/5.1/?.lua"' >> $IROOT/lua.installed
echo -e 'export LUA_CPATH="./?.lua;./?.lc;$LUA_HOME/share/lua/5.1/?/init.so;$LUA_HOME/share/lua/5.1/?.so;$LUA_HOME/lib/lua/5.1/?/init.so;$LUA_HOME/lib/lua/5.1/?.so"' >> $IROOT/lua.installed

source $IROOT/lua.installed
