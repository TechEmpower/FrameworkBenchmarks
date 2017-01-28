#!/bin/bash

fw_depends mongodb python2

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements_mongo.txt

python server_py2.py --port=8080 --mongo=$DBHOST --logging=error &
