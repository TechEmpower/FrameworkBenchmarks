#!/bin/bash
export PY2_ROOT=$IROOT/py2
export PY2=$PY2_ROOT/bin/python
export PY2_PIP=$PY2_ROOT/bin/pip
export PY2_GUNICORN=$PY2_ROOT/bin/gunicorn

$PY2_ROOT/bin/uwsgi --master -L -l 5000 --gevent 1000 --http :8080 --http-keepalive --http-processes ${MAX_THREADS} -p ${MAX_THREADS} -w hello --add-header "Connection: keep-alive" --pidfile /tmp/uwsgi.pid &