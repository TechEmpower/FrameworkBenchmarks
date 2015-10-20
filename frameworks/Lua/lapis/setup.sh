#!/bin/bash

sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' config.lua
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' config.moon
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' nginx.conf

fw_depends lua luarocks openresty

luarocks install lapis
lapis server production &
