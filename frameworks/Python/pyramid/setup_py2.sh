#!/bin/bash

fw_depends postgresql python2

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

gunicorn wsgi:app -c gunicorn_conf.py &
