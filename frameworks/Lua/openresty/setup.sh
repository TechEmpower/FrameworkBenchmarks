#!/bin/bash

sed -i 's|CWD|'"${TROOT}"'|g' nginx.conf
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' app.lua

fw_depends mysql lua luarocks openresty

luarocks install lua-resty-template

nginx -c $TROOT/nginx.conf -g "worker_processes '"${CPU_COUNT}"';" &
