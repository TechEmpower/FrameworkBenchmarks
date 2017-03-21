#!/bin/bash

fw_depends mongodb python3

pip3 install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements_mongo.txt

python3 server_py3.py --port=8080 --mongo=$DBHOST --logging=error &
