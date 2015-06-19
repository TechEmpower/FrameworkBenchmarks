#!/bin/bash

VERSION="5.1.5"
LUA=$IROOT/lua$VERSION
RETCODE=$(fw_exists ${LUA}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $LUA.installed
  return 0; }

fw_get -O https://github.com/LuaDist/lua/archive/5.1.5-Ubuntu-x86_64.tar.gz
fw_untar 5.1.5-Ubuntu-x86_64.tar.gz

LUA_HOME=$IROOT/lua-5.1.5-Ubuntu-x86_64
echo "export LUA_HOME=${LUA_HOME}" > $LUA.installed
echo -e "export PATH=${LUA_HOME}/bin:\$PATH" >> $LUA.installed

source $LUA.installed
