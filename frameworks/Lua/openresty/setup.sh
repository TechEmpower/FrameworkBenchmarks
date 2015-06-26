#!/bin/bash

sed -i 's|CWD|'"${TROOT}"'|g' nginx.conf
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' app.lua

fw_depends lua luarocks openresty 

luarocks install lua-rest-template

nginx -c $TROOT/nginx.conf -g "worker_processes '"${MAX_THREADS}"';" &
