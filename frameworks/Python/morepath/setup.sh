#!/bin/bash

fw_depends postgresql python3

/usr/bin/python3.6 -m venv $TROOT/venv/ --clear

$TROOT/venv/bin/pip install -r $TROOT/requirements.txt

$TROOT/venv/bin/gunicorn app.run -c gunicorn_conf.py &
