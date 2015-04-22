#!/bin/bash

fw_depends pypy

$PYPY_ROOT/bin/pip install --install-option="--prefix=${PYPY_ROOT}" -r $TROOT/requirements-pypy.txt

gunicorn app:app -c gunicorn_conf.py &
