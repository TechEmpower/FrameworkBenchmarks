#!/bin/bash

sed -i 's|include .*/conf/uwsgi_params;|include '"${NGINX_HOME}"'/conf/uwsgi_params;|g' nginx.conf

${NGINX_HOME}/sbin/nginx -c ${TROOT}/nginx.conf
${PY2_ROOT}/bin/uwsgi --ini ${TROOT}/uwsgi.ini --processes ${MAX_THREADS} --wsgi app:app &
