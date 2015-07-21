#!/bin/bash

hhvm --config ./deploy/config.hdf -m server
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf

