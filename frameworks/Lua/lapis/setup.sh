#!/bin/bash

sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' config.lua
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' config.moon
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' nginx.conf

export LAPIS_OPENRESTY=${IROOT}/openresty-1.7.7.1
export PATH=${LAPIS_OPENRESTY}/nginx/sbin:$PATH

lapis server production &