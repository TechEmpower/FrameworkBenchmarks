#!/bin/bash

fw_depends python3

pip3 install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

gunicorn app:app -c gunicorn_conf.py &
