#!/bin/bash

fw_depends python2 nginx

sed -i 's|include .*/conf/uwsgi_params;|include '"${NGINX_HOME}"'/conf/uwsgi_params;|g' nginx.conf

nginx -c $TROOT/nginx.conf
uwsgi --ini uwsgi.ini --processes $MAX_THREADS --wsgi hello:app &
