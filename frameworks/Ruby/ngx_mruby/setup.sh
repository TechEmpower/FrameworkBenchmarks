#!/bin/bash

sed -i 's|CWD|'"${TROOT}"'|g' nginx.conf
sed -i 's|DBHOSTNAME|'"${DBHOST}"'|g' nginx.conf

${IROOT}/nginx_mruby/sbin/nginx -c $TROOT/nginx.conf &
