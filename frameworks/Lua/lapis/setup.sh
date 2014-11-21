#!/bin/bash

sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' config.lua
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' nginx.conf

export PATH=${OPENRESTY_HOME}/nginx/sbin:$PATH

lapis server production &