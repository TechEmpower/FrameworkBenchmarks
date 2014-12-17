#!/bin/bash

$NGINX_HOME/sbin/nginx -c $TROOT/nginx.conf

$PY2_ROOT/bin/uwsgi --ini uwsgi.ini --processes ${MAX_THREADS} --wsgi hello:app &