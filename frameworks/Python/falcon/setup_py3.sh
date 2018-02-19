#!/bin/bash

fw_depends python3

/usr/bin/python3.6 -m venv $TROOT/venv/ --clear

$TROOT/venv/bin/pip install -r $TROOT/requirements.lock

$TROOT/venv/bin/gunicorn app:app -c gunicorn_conf.py &
