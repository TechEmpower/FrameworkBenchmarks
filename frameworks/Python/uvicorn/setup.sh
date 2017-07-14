#!/bin/bash

fw_depends postgresql python3

pip3 install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

uvicorn app:main -c uvicorn_conf.py &
