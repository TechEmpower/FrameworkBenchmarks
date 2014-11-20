#!/bin/bash

sed -i 's|CWD|'"${TROOT}"'|g' nginx.conf
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' app.lua

${OPENRESTY_HOME}/nginx/sbin/nginx -c $TROOT/nginx.conf -g 'worker_processes \x27'"${MAX_THREADS}"'\x27;' &