#!/bin/bash

hhvm --config ./deploy/config-debug.hdf -m server
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf

