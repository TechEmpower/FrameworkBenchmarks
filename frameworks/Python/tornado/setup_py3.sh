#!/bin/bash
export PY3_ROOT=$IROOT/py3
export PY3=$PY3_ROOT/bin/python3
export PY3_PIP=$PY3_ROOT/bin/pip3
export PY3_GUNICORN=$PY3_ROOT/bin/gunicorn

$PY3 server.py --port=8080 --mongo=${DBHOST} --logging=error &