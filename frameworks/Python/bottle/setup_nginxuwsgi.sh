#!/bin/bash
export PY2_ROOT=$IROOT/py2
export PY2=$PY2_ROOT/bin/python
export PY2_PIP=$PY2_ROOT/bin/pip
export PY2_GUNICORN=$PY2_ROOT/bin/gunicorn

export NGINX_HOME=$IROOT/nginx

sed -i 's|include .*/conf/uwsgi_params;|include '"${NGINX_HOME}"'/conf/uwsgi_params;|g' nginx.conf

${NGINX_HOME}/sbin/nginx -c ${TROOT}/nginx.conf
${PY2_ROOT}/bin/uwsgi --ini ${TROOT}/uwsgi.ini --processes ${MAX_THREADS} --wsgi app:app &
