#!/bin/bash

fw_depends postgresql python2

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

python server_pg.py --port=8080 --postgres=$DBHOST --logging=error &
