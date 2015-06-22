#!/bin/bash

hhvm -m server --config ./deploy/config.hdf
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf
