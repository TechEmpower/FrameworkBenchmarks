#!/bin/bash

fw_depends python2

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirementsa.lock

gunicorn app:app -c gunicorn_conf.py &
