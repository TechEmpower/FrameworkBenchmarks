#!/bin/bash

fw_depends pypy

pip install --install-option="--prefix=${PYPY_ROOT}" -r $TROOT/requirements-pypy.txt

gunicorn app:app -c gunicorn_conf.py &
