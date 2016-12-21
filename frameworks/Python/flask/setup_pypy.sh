#!/bin/bash

fw_depends mysql pypy

pip install --install-option="--prefix=${PYPY_ROOT}" -r $TROOT/requirements-pypy.txt

gunicorn app:app -c gunicorn_conf.py &
