#!/bin/bash

fw_depends mysql python3 nginx

sed -i 's|include .*/conf/uwsgi_params;|include '"${NGINX_HOME}"'/conf/uwsgi_params;|g' nginx.conf

pip3 install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

nginx -c $TROOT/nginx.conf
uwsgi --ini $TROOT/uwsgi.ini --processes $((CPU_COUNT*3)) --wsgi app:app &
