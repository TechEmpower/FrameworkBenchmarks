#!/bin/bash

fw_depends mysql python3

pip3 install --install-option="--prefix=${PY3_ROOT}" -r $TROOT/requirements.txt

python3 app.py &
