#!/bin/bash

hhvm -m server --config ./deploy/config-debug.hdf
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf
