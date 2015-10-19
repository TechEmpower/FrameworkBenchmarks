#!/bin/bash

fw_depends python2

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

gunicorn hello:app -c gunicorn_conf.py &
