#!/bin/bash

fw_depends python3

pip install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

python3 server.py --port=8080 --mongo=$DBHOST --logging=error &
