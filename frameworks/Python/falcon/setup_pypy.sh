#!/bin/bash
export PYPY_ROOT=$IROOT/pypy
export PYPY=$PYPY_ROOT/bin/python
export PYPY_PIP=$PYPY_ROOT/bin/pip
export PYPY_GUNICORN=$PYPY_ROOT/bin/gunicorn

$PYPY_GUNICORN app:app -c gunicorn_conf.py &