#!/bin/bash

export PY2_ROOT=$IROOT/py2
export PY2=$PY2_ROOT/bin/python
export PY2_GUNICORN=$PY2_ROOT/bin/gunicorn

$PY2_GUNICORN app:app -c gunicorn_conf.py &
