#!/bin/bash

export PY2_ROOT=$IROOT/py2
export PY2=$PY2_ROOT/bin/python
export PY2_PIP=$PY2_ROOT/bin/pip
export PY2_GUNICORN=$PY2_ROOT/bin/gunicorn

$PY2_GUNICORN wsgi:app -c gunicorn_conf.py &
