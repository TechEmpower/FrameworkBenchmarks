#!/bin/bash

export PWD=$(pwd)

sed -i 's|CWD|'"${PWD}"'|g' nginx.conf
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' app.lua

/usr/local/openresty/nginx/sbin/nginx -c $TROOT/nginx.conf -g "worker_processes '$MAX_THREADS';" &