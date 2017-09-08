#!/bin/bash

fw_depends postgresql pypy2

pip install --install-option="--prefix=${PYPY2_ROOT}" -r $TROOT/requirements-pypy.txt

gunicorn app:app -c gunicorn_conf.py &
