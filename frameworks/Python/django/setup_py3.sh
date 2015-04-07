#!/bin/bash

export PY3_ROOT=$IROOT/py3
export PY3=$PY3_ROOT/bin/python
export PY3_PIP=$PY3_ROOT/bin/pip3
export PY3_GUNICORN=$PY3_ROOT/bin/gunicorn

$PY3_GUNICORN --pid=gunicorn.pid hello.wsgi:application -c gunicorn_conf.py --env DJANGO_DB=mysql &
