#!/bin/bash

fw_depends mongodb python3

/usr/bin/python3.6 -m venv $TROOT/venv/ --clear

$TROOT/venv/bin/pip install -r $TROOT/requirements_mongo.txt

$TROOT/venv/bin/python server_py3.py --port=8080 --mongo=$DBHOST --logging=error &
