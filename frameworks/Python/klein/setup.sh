#!/bin/bash

fw_depends mysql python2

pip install --install-option="--prefix=${PY2_ROOT}" -r $TROOT/requirements.txt

python app.py &
