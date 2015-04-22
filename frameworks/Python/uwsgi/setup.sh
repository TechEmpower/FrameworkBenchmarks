#!/bin/bash

fw_depends python2

$PY2_ROOT/bin/pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

$PY2_ROOT/bin/uwsgi --master -L -l 5000 --gevent 1000 --http :8080 --http-keepalive --http-processes $MAX_THREADS -p $MAX_THREADS -w hello --add-header "Connection: keep-alive" --pidfile /tmp/uwsgi.pid &
