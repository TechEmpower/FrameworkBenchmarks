#!/bin/bash

fw_depends python3

/usr/bin/python3.6 -m venv $TROOT/venv/ --clear

$TROOT/venv/bin/pip install -r $TROOT/requirements.txt

$TROOT/venv/bin/gunicorn app:app.wsgi -c gunicorn_conf.py &
