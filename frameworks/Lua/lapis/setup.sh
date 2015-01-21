#!/bin/bash

sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' config.lua
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' config.moon
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' nginx.conf

export OPENRESTY_HOME=${IROOT}/openresty-1.7.7.1
export LAPIS_OPENRESTY=${IROOT}/openresty-1.7.7.1
export PATH=${OPENRESTY_HOME}/nginx/sbin:$PATH

lapis server production &