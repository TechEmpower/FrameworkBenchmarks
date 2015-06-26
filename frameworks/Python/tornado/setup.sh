#!/bin/bash

fw_depends python2

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

python server.py --port=8080 --mongo=$DBHOST --logging=error &
