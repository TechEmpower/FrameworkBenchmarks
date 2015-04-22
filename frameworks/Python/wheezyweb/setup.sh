#!/bin/bash

fw_depends python2

$PY2_ROOT/bin/pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

gunicorn app:app -c gunicorn_conf.py &
