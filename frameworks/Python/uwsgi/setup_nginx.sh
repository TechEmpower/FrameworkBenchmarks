#!/bin/bash

fw_depends python2 nginx

sed -i 's|include .*/conf/uwsgi_params;|include '"${NGINX_HOME}"'/conf/uwsgi_params;|g' nginx.conf

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

nginx -c $TROOT/nginx.conf

uwsgi --ini uwsgi.ini --processes $MAX_THREADS --gevent 1000 --wsgi hello &
