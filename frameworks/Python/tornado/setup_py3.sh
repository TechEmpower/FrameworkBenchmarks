#!/bin/bash

fw_depends python3

$PY3_ROOT/bin/pip install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

$PY3_ROOT/bin/python3 server.py --port=8080 --mongo=${DBHOST} --logging=error &
