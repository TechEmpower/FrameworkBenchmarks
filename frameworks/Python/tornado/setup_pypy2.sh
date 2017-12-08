#!/bin/bash

fw_depends mongodb pypy2

pip install --install-option="--prefix=${PYPY2_ROOT}" -r $TROOT/requirements_mongo.txt

pypy server_py2.py --port=8080 --mongo=$DBHOST --logging=error &
